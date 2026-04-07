#' Symmetry454 time unit classes
#'
#' Time unit constructors for the Symmetry454 calendar system. These units can be
#' used with [linear_time()] to create custom time representations.
#'
#' @format A civil-based calendar containing Symmetry454 time units.
#'
#' @details
#' The Symmetry454 calendar (Sym454) is a perennial solar calendar proposed by
#' Dr. Irv Bromberg. It preserves the traditional 12-month structure and 7-day
#' week, with months arranged in a symmetrical 4-5-4 week pattern per quarter.
#' Every month starts on Monday and has a whole number of weeks, meaning no
#' month ever contains a partial week.
#' 
#' The following time units are available in the Symmetry454 calendar (`cal_sym454$`).
#'
#' - `year()`: Year unit
#' - `month()`: Month unit
#' - `week()`: Week unit
#' - `day()`: Day unit
#' - `hour()`: Hour unit
#' - `minute()`: Minute unit
#' - `second()`: Second unit
#' - `millisecond()`: Millisecond unit
#'
#' ## Leap years
#'
#' Rather than intercalary days, Symmetry454 uses a **leap week** appended to
#' December once every 5 or 6 years, making December a 5-week month in leap
#' years. A year is a leap year if `(52 * year + 146) %% 293 < 52`.
#'
#' This yields a mean year of 365 + 71/293 days (≈ 365 days, 5 hours, 48
#' minutes, 56.5 seconds), intentionally slightly shorter than the mean
#' northward equinoctial year.
#'
#' @seealso [linear_time()] for creating linear time points, and
#'   <https://en.wikipedia.org/wiki/Symmetry454> for more calendar details.
#'
#' @examples
#' # Create a custom time representation using Symmetry454 units
#' linear_time(
#'   Sys.time(),
#'   chronon = cal_sym454$week(1L)
#' )
#'
#' @name calendar_sym454
#' @export
cal_sym454 <- new_calendar(
  year = new_class("tu_sym454_year", parent = mt_tz_unit),
  month = new_class("tu_sym454_month", parent = mt_tz_unit),
  week = cal_isoweek$week,
  inherit = cal_time_civil_midnight,
  class = "cal_sym454"
)

# Time unit labels
## Default formats
S7::method(
  chronon_format_linear,
  list(cal_sym454$year, S7::class_any)
) <- function(x, cal) "{lin(year(1L))}"
S7::method(
  chronon_format_linear,
  list(cal_sym454$month, S7::class_any)
) <- function(x, cal) "{lin(year(1L))}-{cyc(month(1L), year(1L), label=TRUE, abbreviate=TRUE)}"
S7::method(
  chronon_format_linear,
  list(cal_sym454$week, S7::new_S3_class("cal_sym454"))
) <- function(x, cal) "{lin(year(1L))}-{cyc(month(1L), year(1L), label=TRUE, abbreviate=TRUE)}-W{cyc(week(1L), month(1L))}"
S7::method(
  chronon_format_linear,
  list(cal_sym454$day, S7::new_S3_class("cal_sym454"))
) <- function(x, cal) "{lin(year(1L))}-{cyc(month(1L), year(1L), label=TRUE, abbreviate=TRUE)}-W{cyc(week(1L), month(1L))}-{cyc(day(1L), week(1L), label=TRUE, abbreviate=TRUE)}"

## Time labels
S7::method(linear_labels, cal_sym454$year) <- function(granule, i, ...) {
  ifelse(i <= 0L, paste0(-i + 1L, "BC"), i)
}
S7::method(cyclical_labels, list(cal_sym454$month, cal_sym454$year)) <-
  function(granule, cycle, i, label = FALSE, abbreviate = FALSE, ...) {
    if (label) {
      # Index into R's localised month name objects (month.name and month.abb)
      if (abbreviate) month.abb[i + 1L] else month.name[i + 1L]
    } else {
      # Use i + 1L for 1-indexing months (so January is 1, February is 2, ...)
      sprintf("%02d", i + 1L)
    }
  }
S7::method(cyclical_labels, list(cal_sym454$week, cal_sym454$month)) <-
  function(granule, cycle, i, ...) {
    as.character(i + 1L)
  }

# Calendar arithmetic
## Epochs
method(chronon_epoch, cal_sym454$year) <- function(x) 1970L

## Cardinality
S7::method(chronon_cardinality, list(cal_sym454$month, cal_sym454$year)) <-
  function(x, y, at = NULL) {
    vctrs::vec_data(y) * 12L / vctrs::vec_data(x)
  }
S7::method(chronon_cardinality, list(cal_sym454$week, cal_sym454$month)) <-
  function(x, y, at = NULL) {
    # The number of weeks in each n-month period
    month_size <- vctrs::vec_data(y)
    nweeks_cyc <- circsum(c(4L, 5L, 4L), month_size)

    # Find which n-month period we're in based on the "at" position (months since epoch)
    period <- at %% length(nweeks_cyc) + 1L

    nweeks <- nweeks_cyc[period]

    # Add the extra week to December for Symmetry454 leap years.
    # A year is a leap year when (52*year + 146) %% 293 < 52.
    m1 <- at * month_size
    contains_dec <- which((m1 %% 12L) >= (12L - month_size))
    year <- 1970L + m1[contains_dec] %/% 12L
    is_leap_year <- ((52 * year + 146L) %% 293L) < 52L

    nweeks[contains_dec[is_leap_year]] <- nweeks[contains_dec[is_leap_year]] + 1L

    # Scale by the number of weeks in the week time unit
    nweeks / vctrs::vec_data(x)
  }

## Divmod
S7::method(chronon_divmod, list(cal_sym454$week, cal_sym454$month)) <-
  function(from, to, x) {
    # Most of this code works on 1-week units
    week_size <- vctrs::vec_data(from)
    x <- x * week_size  # convert n-weeks to 1-weeks

    # 1. Account for leap weeks by regularising x to have a fixed 52 weeks per year

    # The symmetrical sub-cycles of the 293-year leap week cycle are:
    #   17+11+17 + 17+17+11+17+17 + 17+11+17 + 17+17+11+17+17 + 17+11+17
    #   = 45 + 79 + 45 + 79 + 45 = 293
    # Primary (length 17) sub-cycles have 3 leap years: 00100000100000100
    # Secondary (length 11) sub-cycles have 2 leap years: 00100000100
    leaps_cycle_17 <- function(w) (w >= 157L) + (w >= 470L) + (w >= 783L)
    leaps_cycle_11 <- function(w) (w >= 157L) + (w >= 470L)
    leaps_cycle_45 <- function(w) {
      ifelse(w < 887L,  leaps_cycle_17(w),
      ifelse(w < 1461L, 3L + leaps_cycle_11(w - 887L),
                        5L + leaps_cycle_17(w - 1461L)))
    }
    leaps_cycle_79 <- function(w) {
      ifelse(w < 887L,  leaps_cycle_17(w),
      ifelse(w < 1774L, 3L  + leaps_cycle_17(w - 887L),
      ifelse(w < 2348L, 6L  + leaps_cycle_11(w - 1774L),
      ifelse(w < 3235L, 8L  + leaps_cycle_17(w - 2348L),
                        11L + leaps_cycle_17(w - 3235L)))))
    }
    leaps_symmetry454 <- function(x) {
      # There are 15288 weeks in a full 293-year cycle (293*52 + 52 leap weeks)
      w <- x %% 15288L
      x %/% 15288L * 52L +
      ifelse(w < 2348L,  leaps_cycle_45(w),
      ifelse(w < 6470L,  8L  + leaps_cycle_79(w - 2348L),
      ifelse(w < 8818L,  22L + leaps_cycle_45(w - 6470L),
      ifelse(w < 12940L, 30L + leaps_cycle_79(w - 8818L),
                         44L + leaps_cycle_45(w - 12940L)))))
    }

    # Offset x to align with the nearest 293-year cycle boundary before the epoch.
    # There are 349 leap years between year 1-W1 and the 1970-W1 epoch, and the
    # nearest cycle start is (1969*52 + 349) %% (293*52 + 52) = 11009 weeks before epoch.
    x_cyc <- x + 11009L + week_size  # right align multi-week units
    n_leaps <- leaps_symmetry454(x_cyc)

    # Regularise x to have exactly 52 weeks per year by subtracting leap weeks.
    # (37 leap years occur in the cycle before the epoch, so we add 37 back.)
    x_reg <- x - n_leaps + 37L

    # 2. Use the 4-5-4 pattern to find the month (div) and week remainder (mod)
    ## The number of weeks in each n-month period
    month_size <- vctrs::vec_data(to)
    weeks_len <- circsum(c(4L, 5L, 4L), month_size)

    ## The total weeks in a full n-month cycle
    weeks_tot <- sum(weeks_len)

    ## Find which n-month cycle we're in based on the regularised week count
    period_full <- x_reg %/% weeks_tot

    ## Find which part within the n-month cycle we're in
    weeks_seq  <- cumsum(weeks_len[-length(weeks_len)])
    period_part <- rowSums(outer(x_reg %% weeks_tot, weeks_seq, ">="))

    # div: total complete n-month cycles + complete n-months within the current cycle
    div <- period_full * length(weeks_len) + period_part
    # mod: remaining (regularised) weeks within the current n-month period
    mod <- x_reg %% weeks_tot - c(0L, weeks_seq)[period_part + 1L]


    # 3. Adjust the remainder to account for leap weeks that were removed during regularisation.

    # Identify leap weeks re-using cumulative in-cycle leap week counts: leaps_symmetry454()
    # If the week added a leap week from the previous, then it itself must be a leap week.
    # Applied only to regularised 52/53rd weeks of the year (only these weeks can be leap weeks)
    last_weeks  <- which(x_reg %% 52L >= 51L)
    leap_weeks  <- last_weeks[n_leaps[last_weeks] - leaps_symmetry454(x_cyc[last_weeks] - week_size) > 0L]
    mod[leap_weeks] <- mod[leap_weeks] + 1L  # restore leap week to remainder

    # Scale mod back to the original n-unit week size
    mod <- mod %/% week_size

    # Return the divmod result
    list(div = div, mod = mod)
  }