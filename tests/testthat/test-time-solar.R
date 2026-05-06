date1 <- as.integer(as.Date("2026-02-26"))
date2 <- as.integer(as.Date("2026-08-19"))
expect_equal_time <- function(object, expected, tolerance = 120, ...) {
  expect_equal(as.numeric(object), as.numeric(expected), tolerance = tolerance, ...)
}

# ---------------------------------------------------------------------------
# Phase boundary index (new altitude-event scheme):
#
#  p=0  b=0  astro dawn    −18° morning  (astro twilight begins, night ends)
#  p=1  b=1  nautical dawn −12° morning
#  p=2  b=2  civil dawn    −6°  morning
#  p=3  b=3  sunrise       −0.833° morning (day begins)
#  p=4  b=4  sunset        −0.833° evening (day ends)
#  p=5  b=5  civil dusk    −6°  evening
#  p=6  b=6  nautical dusk −12° evening
#  p=7  b=7  astro dusk    −18° evening  (night begins)
#
#  Night (p=7) straddles solar midnight: astro dusk to next day's astro dawn.
#  Phase count for UTC day d, phase p: d * 8L + p
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# All 8 phase boundaries: Melbourne, date1 (2026-02-26, AEDT = UTC+11)
#
# Critical edge case: Melbourne solar noon is ~02:30 UTC, so morning events
# (p=0..3) occur on UTC Feb 25 even though the solar day is indexed as Feb 26.
# ---------------------------------------------------------------------------
test_that("All 8 phase boundaries correct: Melbourne, 2026-02-26", {
  mel <- cal_time_solar$illumination(1L, lat = -37.8136, lon = 144.9631)

  # p=0: astronomical dawn (sun crosses −18° ascending)
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 0L, mel)),
    as.POSIXct("2026-02-26 05:37:00", tz = "Australia/Melbourne")
  )
  # p=1: nautical dawn (sun crosses −12° ascending)
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 1L, mel)),
    as.POSIXct("2026-02-26 06:07:00", tz = "Australia/Melbourne")
  )
  # p=2: civil dawn (sun crosses −6° ascending)
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 2L, mel)),
    as.POSIXct("2026-02-26 06:34:00", tz = "Australia/Melbourne")
  )
  # p=3: sunrise (sun crosses −0.833° ascending)
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 3L, mel)),
    as.POSIXct("2026-02-26 07:01:00", tz = "Australia/Melbourne")
  )
  # p=4: sunset (sun crosses −0.833° descending)
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 4L, mel)),
    as.POSIXct("2026-02-26 20:04:00", tz = "Australia/Melbourne")
  )
  # p=5: civil dusk (sun crosses −6° descending)
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 5L, mel)),
    as.POSIXct("2026-02-26 20:31:00", tz = "Australia/Melbourne")
  )
  # p=6: nautical dusk (sun crosses −12° descending)
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 6L, mel)),
    as.POSIXct("2026-02-26 20:58:00", tz = "Australia/Melbourne")
  )
  # p=7: astronomical dusk / night begins (sun crosses −18° descending)
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 7L, mel)),
    as.POSIXct("2026-02-26 21:28:00", tz = "Australia/Melbourne")
  )
})

# ---------------------------------------------------------------------------
# All 8 phase boundaries: Melbourne, date2 (2026-08-19, AEST = UTC+10)
# Southern Hemisphere winter: shorter days, different twilight lengths.
# ---------------------------------------------------------------------------
test_that("All 8 phase boundaries correct: Melbourne, 2026-08-19", {
  mel <- cal_time_solar$illumination(1L, lat = -37.8136, lon = 144.9631)

  # p=0: astronomical dawn
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 0L, mel)),
    as.POSIXct("2026-08-19 05:36:00", tz = "Australia/Melbourne")
  )
  # p=1: nautical dawn
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 1L, mel)),
    as.POSIXct("2026-08-19 06:06:00", tz = "Australia/Melbourne")
  )
  # p=2: civil dawn
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 2L, mel)),
    as.POSIXct("2026-08-19 06:33:00", tz = "Australia/Melbourne")
  )
  # p=3: sunrise
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 3L, mel)),
    as.POSIXct("2026-08-19 07:00:00", tz = "Australia/Melbourne")
  )
  # p=4: sunset
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 4L, mel)),
    as.POSIXct("2026-08-19 17:47:00", tz = "Australia/Melbourne")
  )
  # p=5: civil dusk
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 5L, mel)),
    as.POSIXct("2026-08-19 18:14:00", tz = "Australia/Melbourne")
  )
  # p=6: nautical dusk
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 6L, mel)),
    as.POSIXct("2026-08-19 18:41:00", tz = "Australia/Melbourne")
  )
  # p=7: astronomical dusk / night begins
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 7L, mel)),
    as.POSIXct("2026-08-19 19:11:00", tz = "Australia/Melbourne")
  )
})

# ---------------------------------------------------------------------------
# All 8 phase boundaries: Munich, date1 (2026-02-26, CET = UTC+1)
# Northern Hemisphere, late winter.
# ---------------------------------------------------------------------------
test_that("All 8 phase boundaries correct: Munich, 2026-02-26", {
  muc <- cal_time_solar$illumination(1L, lat = 48.133, lon = 11.583)

  # p=0: astronomical dawn
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 0L, muc)),
    as.POSIXct("2026-02-26 05:18:00", tz = "Europe/Berlin")
  )
  # p=1: nautical dawn
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 1L, muc)),
    as.POSIXct("2026-02-26 05:58:00", tz = "Europe/Berlin")
  )
  # p=2: civil dawn
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 2L, muc)),
    as.POSIXct("2026-02-26 06:29:00", tz = "Europe/Berlin")
  )
  # p=3: sunrise
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 3L, muc)),
    as.POSIXct("2026-02-26 07:00:00", tz = "Europe/Berlin")
  )
  # p=4: sunset
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 4L, muc)),
    as.POSIXct("2026-02-26 17:53:00", tz = "Europe/Berlin")
  )
  # p=5: civil dusk
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 5L, muc)),
    as.POSIXct("2026-02-26 18:24:00", tz = "Europe/Berlin")
  )
  # p=6: nautical dusk
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 6L, muc)),
    as.POSIXct("2026-02-26 18:55:00", tz = "Europe/Berlin")
  )
  # p=7: astronomical dusk / night begins
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 7L, muc)),
    as.POSIXct("2026-02-26 19:37:00", tz = "Europe/Berlin")
  )
})

# ---------------------------------------------------------------------------
# All 8 phase boundaries: Munich, date2 (2026-08-19, CEST = UTC+2)
# Northern Hemisphere, late summer: long days and notably extended twilights
# at 48°N (civil ~34 min, nautical ~46 min, astro ~66 min per band).
# ---------------------------------------------------------------------------
test_that("All 8 phase boundaries correct: Munich, 2026-08-19", {
  muc <- cal_time_solar$illumination(1L, lat = 48.133, lon = 11.583)

  # p=0: astronomical dawn
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 0L, muc)),
    as.POSIXct("2026-08-19 04:05:00", tz = "Europe/Berlin")
  )
  # p=1: nautical dawn
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 1L, muc)),
    as.POSIXct("2026-08-19 05:05:00", tz = "Europe/Berlin")
  )
  # p=2: civil dawn
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 2L, muc)),
    as.POSIXct("2026-08-19 05:39:00", tz = "Europe/Berlin")
  )
  # p=3: sunrise
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 3L, muc)),
    as.POSIXct("2026-08-19 06:13:00", tz = "Europe/Berlin")
  )
  # p=4: sunset
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 4L, muc)),
    as.POSIXct("2026-08-19 20:20:00", tz = "Europe/Berlin")
  )
  # p=5: civil dusk
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 5L, muc)),
    as.POSIXct("2026-08-19 20:53:00", tz = "Europe/Berlin")
  )
  # p=6: nautical dusk
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 6L, muc)),
    as.POSIXct("2026-08-19 21:27:00", tz = "Europe/Berlin")
  )
  # p=7: astronomical dusk / night begins
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 7L, muc)),
    as.POSIXct("2026-08-19 22:27:00", tz = "Europe/Berlin")
  )
})

# ---------------------------------------------------------------------------
# All 8 phase boundaries: Honolulu (Hawaii), date1 (2026-02-26, HST = UTC-10)
# West-longitude location; Hawaii does not observe DST.
# ---------------------------------------------------------------------------
test_that("All 8 phase boundaries correct: Honolulu, 2026-02-26", {
  hni <- cal_time_solar$illumination(1L, lat = 21.3069, lon = -157.8583)

  # p=0: astronomical dawn
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 0L, hni)),
    as.POSIXct("2026-02-26 05:40:00", tz = "Pacific/Honolulu")
  )
  # p=1: nautical dawn
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 1L, hni)),
    as.POSIXct("2026-02-26 06:06:00", tz = "Pacific/Honolulu")
  )
  # p=2: civil dawn
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 2L, hni)),
    as.POSIXct("2026-02-26 06:32:00", tz = "Pacific/Honolulu")
  )
  # p=3: sunrise
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 3L, hni)),
    as.POSIXct("2026-02-26 06:54:00", tz = "Pacific/Honolulu")
  )
  # p=4: sunset
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 4L, hni)),
    as.POSIXct("2026-02-26 18:34:00", tz = "Pacific/Honolulu")
  )
  # p=5: civil dusk
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 5L, hni)),
    as.POSIXct("2026-02-26 18:57:00", tz = "Pacific/Honolulu")
  )
  # p=6: nautical dusk
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 6L, hni)),
    as.POSIXct("2026-02-26 19:23:00", tz = "Pacific/Honolulu")
  )
  # p=7: astronomical dusk / night begins
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 7L, hni)),
    as.POSIXct("2026-02-26 19:48:00", tz = "Pacific/Honolulu")
  )
})

# ---------------------------------------------------------------------------
# All 8 phase boundaries: Honolulu (Hawaii), date2 (2026-08-19, HST = UTC-10)
# Northern Hemisphere summer: longer days than February.
# ---------------------------------------------------------------------------
test_that("All 8 phase boundaries correct: Honolulu, 2026-08-19", {
  hni <- cal_time_solar$illumination(1L, lat = 21.3069, lon = -157.8583)

  # p=0: astronomical dawn
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 0L, hni)),
    as.POSIXct("2026-08-19 04:54:00", tz = "Pacific/Honolulu")
  )
  # p=1: nautical dawn
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 1L, hni)),
    as.POSIXct("2026-08-19 05:21:00", tz = "Pacific/Honolulu")
  )
  # p=2: civil dawn
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 2L, hni)),
    as.POSIXct("2026-08-19 05:48:00", tz = "Pacific/Honolulu")
  )
  # p=3: sunrise
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 3L, hni)),
    as.POSIXct("2026-08-19 06:11:00", tz = "Pacific/Honolulu")
  )
  # p=4: sunset
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 4L, hni)),
    as.POSIXct("2026-08-19 18:59:00", tz = "Pacific/Honolulu")
  )
  # p=5: civil dusk
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 5L, hni)),
    as.POSIXct("2026-08-19 19:22:00", tz = "Pacific/Honolulu")
  )
  # p=6: nautical dusk
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 6L, hni)),
    as.POSIXct("2026-08-19 19:49:00", tz = "Pacific/Honolulu")
  )
  # p=7: astronomical dusk / night begins
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 7L, hni)),
    as.POSIXct("2026-08-19 20:16:00", tz = "Pacific/Honolulu")
  )
})

# ---------------------------------------------------------------------------
# Night phase spans midnight correctly: Honolulu
# ---------------------------------------------------------------------------
test_that("Night phase spans midnight correctly: Honolulu", {
  hni <- cal_time_solar$illumination(1L, lat = 21.3069, lon = -157.8583)

  hni_midnight_d1 <- as.POSIXct("2026-02-27 00:00:00", tz = "Pacific/Honolulu")
  hni_midnight_d2 <- as.POSIXct("2026-08-20 00:00:00", tz = "Pacific/Honolulu")

  # date1: p=7 (astro dusk) is before midnight
  expect_lt(
    as.numeric(as.POSIXct(linear_time(date1 * 8L + 7L, hni))),
    as.numeric(hni_midnight_d1)
  )
  # date1: next day p=0 (astro dawn) is after midnight
  expect_gt(
    as.numeric(as.POSIXct(linear_time((date1 + 1L) * 8L + 0L, hni))),
    as.numeric(hni_midnight_d1)
  )

  # date2: p=7 (astro dusk) is before midnight
  expect_lt(
    as.numeric(as.POSIXct(linear_time(date2 * 8L + 7L, hni))),
    as.numeric(hni_midnight_d2)
  )
  # date2: next day p=0 (astro dawn) is after midnight
  expect_gt(
    as.numeric(as.POSIXct(linear_time((date2 + 1L) * 8L + 0L, hni))),
    as.numeric(hni_midnight_d2)
  )
})

# ---------------------------------------------------------------------------
# Solar noon calculations (unchanged: uses ampm unit, not illumination phase)
# ---------------------------------------------------------------------------
test_that("Solar noon calculations", {
  # Noon = ampm half 1 (start of PM half = solar transit)
  mel_ampm <- cal_time_solar$ampm(1L, lat = -37.8136, lon = 144.9631)
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 2L + 1L, mel_ampm)),
    as.POSIXct("2026-02-26 13:33:00", tz = "Australia/Melbourne")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 2L + 1L, mel_ampm)),
    as.POSIXct("2026-08-19 12:23:00", tz = "Australia/Melbourne")
  )

  muc_ampm <- cal_time_solar$ampm(1L, lat = 48.133, lon = 11.583)
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 2L + 1L, muc_ampm)),
    as.POSIXct("2026-02-26 12:26:00", tz = "Europe/Berlin")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 2L + 1L, muc_ampm)),
    as.POSIXct("2026-08-19 13:17:00", tz = "Europe/Berlin")
  )
})

# ---------------------------------------------------------------------------
# Solar astronomical dawn calculations (replaces "Solar midnight calculations")
# p=0 is the astro-dawn boundary (sun crosses −18° ascending). For Melbourne
# (UTC+11) this falls on UTC day d-1 even though the local date is day d.
# ---------------------------------------------------------------------------
test_that("Solar astronomical dawn calculations", {
  # Astro dawn = illumination phase 0 (sun crosses −18° ascending)
  mel_illumination <- cal_time_solar$illumination(1L, lat = -37.8136, lon = 144.9631)
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 0L, mel_illumination)),
    as.POSIXct("2026-02-26 05:37:00", tz = "Australia/Melbourne")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 0L, mel_illumination)),
    as.POSIXct("2026-08-19 05:36:00", tz = "Australia/Melbourne")
  )

  muc_illumination <- cal_time_solar$illumination(1L, lat = 48.133, lon = 11.583)
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 0L, muc_illumination)),
    as.POSIXct("2026-02-26 05:18:00", tz = "Europe/Berlin")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 0L, muc_illumination)),
    as.POSIXct("2026-08-19 04:05:00", tz = "Europe/Berlin")
  )
})

# ---------------------------------------------------------------------------
# Solar dawn calculations (civil twilight)
# p=2: sun crosses −6° ascending. Old tests used +3L (off-by-one), fixed here.
# ---------------------------------------------------------------------------
test_that("Solar dawn calculations (civil twilight)", {
  # Civil dawn = illumination phase 2 (sun crosses −6° ascending)
  mel_illumination <- cal_time_solar$illumination(1L, lat = -37.8136, lon = 144.9631)
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 2L, mel_illumination)),
    as.POSIXct("2026-02-26 06:34:00", tz = "Australia/Melbourne")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 2L, mel_illumination)),
    as.POSIXct("2026-08-19 06:33:00", tz = "Australia/Melbourne")
  )

  muc_illumination <- cal_time_solar$illumination(1L, lat = 48.133, lon = 11.583)
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 2L, muc_illumination)),
    as.POSIXct("2026-02-26 06:29:00", tz = "Europe/Berlin")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 2L, muc_illumination)),
    as.POSIXct("2026-08-19 05:39:00", tz = "Europe/Berlin")
  )
})

# ---------------------------------------------------------------------------
# Solar sunrise calculations
# p=3: sun crosses −0.833° ascending. Old tests used +4L, fixed here.
# ---------------------------------------------------------------------------
test_that("Solar sunrise calculations", {
  # Sunrise = illumination phase 3 (sun crosses −0.833° ascending)
  mel_illumination <- cal_time_solar$illumination(1L, lat = -37.8136, lon = 144.9631)
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 3L, mel_illumination)),
    as.POSIXct("2026-02-26 07:01:00", tz = "Australia/Melbourne")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 3L, mel_illumination)),
    as.POSIXct("2026-08-19 07:00:00", tz = "Australia/Melbourne")
  )

  muc_illumination <- cal_time_solar$illumination(1L, lat = 48.133, lon = 11.583)
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 3L, muc_illumination)),
    as.POSIXct("2026-02-26 07:00:00", tz = "Europe/Berlin")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 3L, muc_illumination)),
    as.POSIXct("2026-08-19 06:13:00", tz = "Europe/Berlin")
  )
})

# ---------------------------------------------------------------------------
# Solar sunset calculations
# p=4: sun crosses −0.833° descending. Old tests used +5L, fixed here.
# ---------------------------------------------------------------------------
test_that("Solar sunset calculations", {
  # Sunset = illumination phase 4 (sun crosses −0.833° descending)
  mel_illumination <- cal_time_solar$illumination(1L, lat = -37.8136, lon = 144.9631)
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 4L, mel_illumination)),
    as.POSIXct("2026-02-26 20:04:00", tz = "Australia/Melbourne")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 4L, mel_illumination)),
    as.POSIXct("2026-08-19 17:47:00", tz = "Australia/Melbourne")
  )

  muc_illumination <- cal_time_solar$illumination(1L, lat = 48.133, lon = 11.583)
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 4L, muc_illumination)),
    as.POSIXct("2026-02-26 17:53:00", tz = "Europe/Berlin")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 4L, muc_illumination)),
    as.POSIXct("2026-08-19 20:20:00", tz = "Europe/Berlin")
  )
})

# ---------------------------------------------------------------------------
# Solar dusk calculations (civil twilight)
# p=5: sun crosses −6° descending. Old tests used +6L, fixed here.
# ---------------------------------------------------------------------------
test_that("Solar dusk calculations (civil twilight)", {
  # Civil dusk = illumination phase 5 (sun crosses −6° descending)
  mel_illumination <- cal_time_solar$illumination(1L, lat = -37.8136, lon = 144.9631)
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 5L, mel_illumination)),
    as.POSIXct("2026-02-26 20:31:00", tz = "Australia/Melbourne")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 5L, mel_illumination)),
    as.POSIXct("2026-08-19 18:14:00", tz = "Australia/Melbourne")
  )

  muc_illumination <- cal_time_solar$illumination(1L, lat = 48.133, lon = 11.583)
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 5L, muc_illumination)),
    as.POSIXct("2026-02-26 18:24:00", tz = "Europe/Berlin")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 5L, muc_illumination)),
    as.POSIXct("2026-08-19 20:53:00", tz = "Europe/Berlin")
  )
})

# ---------------------------------------------------------------------------
# Night phase spans midnight correctly
#
# p=7 (astro dusk / night start) must be before local midnight of day d.
# p=0 of day d+1 (next astro dawn) must be after local midnight of day d+1.
# This confirms night straddles solar midnight.
# ---------------------------------------------------------------------------
test_that("Night phase spans midnight correctly", {
  mel <- cal_time_solar$illumination(1L, lat = -37.8136, lon = 144.9631)
  muc <- cal_time_solar$illumination(1L, lat = 48.133,   lon = 11.583)

  mel_midnight_d1  <- as.POSIXct("2026-02-27 00:00:00", tz = "Australia/Melbourne")
  mel_midnight_d2  <- as.POSIXct("2026-08-20 00:00:00", tz = "Australia/Melbourne")
  muc_midnight_d1  <- as.POSIXct("2026-02-27 00:00:00", tz = "Europe/Berlin")
  muc_midnight_d2  <- as.POSIXct("2026-08-20 00:00:00", tz = "Europe/Berlin")

  # Melbourne date1: p=7 (astro dusk) is before midnight
  expect_lt(
    as.numeric(as.POSIXct(linear_time(date1 * 8L + 7L, mel))),
    as.numeric(mel_midnight_d1)
  )
  # Melbourne date1: next day p=0 (astro dawn) is after midnight
  expect_gt(
    as.numeric(as.POSIXct(linear_time((date1 + 1L) * 8L + 0L, mel))),
    as.numeric(mel_midnight_d1)
  )

  # Melbourne date2: p=7 (astro dusk) is before midnight
  expect_lt(
    as.numeric(as.POSIXct(linear_time(date2 * 8L + 7L, mel))),
    as.numeric(mel_midnight_d2)
  )
  # Melbourne date2: next day p=0 (astro dawn) is after midnight
  expect_gt(
    as.numeric(as.POSIXct(linear_time((date2 + 1L) * 8L + 0L, mel))),
    as.numeric(mel_midnight_d2)
  )

  # Munich date1: p=7 (astro dusk) is before midnight
  expect_lt(
    as.numeric(as.POSIXct(linear_time(date1 * 8L + 7L, muc))),
    as.numeric(muc_midnight_d1)
  )
  # Munich date1: next day p=0 (astro dawn) is after midnight
  expect_gt(
    as.numeric(as.POSIXct(linear_time((date1 + 1L) * 8L + 0L, muc))),
    as.numeric(muc_midnight_d1)
  )

  # Munich date2: p=7 (astro dusk) is before midnight
  expect_lt(
    as.numeric(as.POSIXct(linear_time(date2 * 8L + 7L, muc))),
    as.numeric(muc_midnight_d2)
  )
  # Munich date2: next day p=0 (astro dawn) is after midnight
  expect_gt(
    as.numeric(as.POSIXct(linear_time((date2 + 1L) * 8L + 0L, muc))),
    as.numeric(muc_midnight_d2)
  )
})

# ---------------------------------------------------------------------------
# Round-trip: UTC → phase count → UTC
#
# Take a known UTC POSIXct at roughly the midpoint of each phase, convert to
# a continuous phase count via approx_solar_phase_from_utc, then back to UTC
# via approx_solar_phase_utc, and verify the result matches the input within
# tolerance. Tests Melbourne date1 and Munich date1 across all 8 phases.
# ---------------------------------------------------------------------------
test_that("Round-trip UTC → phase count → UTC", {
  lat <- -37.8136; lon <- 144.9631

  # Phase midpoints: local Melbourne time (AEDT) for solar day date1
  midpoints_utc <- as.numeric(as.POSIXct(c(
    "2026-02-26 05:52:00",  # p=0 midpoint: between astro dawn and nautical dawn
    "2026-02-26 06:21:00",  # p=1 midpoint: between nautical dawn and civil dawn
    "2026-02-26 06:47:00",  # p=2 midpoint: between civil dawn and sunrise
    "2026-02-26 13:33:00",  # p=3 midpoint: solar noon (midpoint of day)
    "2026-02-26 20:17:00",  # p=4 midpoint: between sunset and civil dusk
    "2026-02-26 20:44:00",  # p=5 midpoint: between civil dusk and nautical dusk
    "2026-02-26 21:13:00",  # p=6 midpoint: between nautical dusk and astro dusk
    "2026-02-27 01:33:00"   # p=7 midpoint: solar midnight (middle of night)
  ), tz = "Australia/Melbourne"))

  phase <- approx_solar_phase_from_utc(midpoints_utc, lat_deg = lat, lon_deg = lon, alt_deg = 0)
  recovered <- approx_solar_phase_utc(phase, lat_deg = lat, lon_deg = lon, alt_deg = 0)
  expect_equal(recovered, midpoints_utc, tolerance = 120)


  lat <- 48.133; lon <- 11.583

  # Phase midpoints: local Munich time (CET) for solar day date1
  midpoints_utc <- as.numeric(as.POSIXct(c(
    "2026-02-26 05:38:00",  # p=0 midpoint: between astro dawn and nautical dawn
    "2026-02-26 06:13:00",  # p=1 midpoint: between nautical dawn and civil dawn
    "2026-02-26 06:44:00",  # p=2 midpoint: between civil dawn and sunrise
    "2026-02-26 12:26:00",  # p=3 midpoint: solar noon (midpoint of day)
    "2026-02-26 18:08:00",  # p=4 midpoint: between sunset and civil dusk
    "2026-02-26 18:39:00",  # p=5 midpoint: between civil dusk and nautical dusk
    "2026-02-26 19:16:00",  # p=6 midpoint: between nautical dusk and astro dusk
    "2026-02-27 00:26:00"   # p=7 midpoint: solar midnight (middle of night)
  ), tz = "Europe/Berlin"))

  phase <- approx_solar_phase_from_utc(midpoints_utc, lat_deg = lat, lon_deg = lon, alt_deg = 0)
  recovered <- approx_solar_phase_utc(phase, lat_deg = lat, lon_deg = lon, alt_deg = 0)
  expect_equal(recovered, midpoints_utc, tolerance = 120)


  lat <- 21.3069; lon <- -157.8583

  # Phase midpoints: local Honolulu time (HST) for solar day date1
  midpoints_utc <- as.numeric(as.POSIXct(c(
    "2026-02-26 05:53:00",  # p=0 midpoint: between astro dawn and nautical dawn
    "2026-02-26 06:19:00",  # p=1 midpoint: between nautical dawn and civil dawn
    "2026-02-26 06:43:00",  # p=2 midpoint: between civil dawn and sunrise
    "2026-02-26 12:44:00",  # p=3 midpoint: solar noon (midpoint of day)
    "2026-02-26 18:46:00",  # p=4 midpoint: between sunset and civil dusk
    "2026-02-26 19:10:00",  # p=5 midpoint: between civil dusk and nautical dusk
    "2026-02-26 19:36:00",  # p=6 midpoint: between nautical dusk and astro dusk
    "2026-02-27 00:44:00"   # p=7 midpoint: solar midnight (middle of night)
  ), tz = "Pacific/Honolulu"))

  phase <- approx_solar_phase_from_utc(midpoints_utc, lat_deg = lat, lon_deg = lon, alt_deg = 0)
  recovered <- approx_solar_phase_utc(phase, lat_deg = lat, lon_deg = lon, alt_deg = 0)
  expect_equal(recovered, midpoints_utc, tolerance = 120)
})

# ---------------------------------------------------------------------------
# Fractional phase interpolation
#
# d * 8 + 3.5 is the midpoint of the day phase (between sunrise and sunset).
# This should be close to solar noon (within a few minutes), since solar noon
# is the midpoint of the day by definition.
# ---------------------------------------------------------------------------
test_that("Fractional phase interpolation: day midpoint ≈ solar noon", {
  mel_illum <- cal_time_solar$illumination(1L, lat = -37.8136, lon = 144.9631)
  mel_ampm  <- cal_time_solar$ampm(1L,         lat = -37.8136, lon = 144.9631)
  muc_illum <- cal_time_solar$illumination(1L, lat = 48.133,   lon = 11.583)
  muc_ampm  <- cal_time_solar$ampm(1L,         lat = 48.133,   lon = 11.583)

  # Melbourne date1: midpoint of day phase vs solar noon
  mel_day_mid <- as.POSIXct(linear_time(date1 * 8 + 3.5, mel_illum))
  mel_noon    <- as.POSIXct(linear_time(date1 * 2 + 1,   mel_ampm))
  expect_equal(as.numeric(mel_day_mid), as.numeric(mel_noon), tolerance = 300)

  # Melbourne date2
  mel_day_mid2 <- as.POSIXct(linear_time(date2 * 8 + 3.5, mel_illum))
  mel_noon2    <- as.POSIXct(linear_time(date2 * 2 + 1,   mel_ampm))
  expect_equal(as.numeric(mel_day_mid2), as.numeric(mel_noon2), tolerance = 300)

  # Munich date1
  muc_day_mid <- as.POSIXct(linear_time(date1 * 8 + 3.5, muc_illum))
  muc_noon    <- as.POSIXct(linear_time(date1 * 2 + 1,   muc_ampm))
  expect_equal(as.numeric(muc_day_mid), as.numeric(muc_noon), tolerance = 300)

  # Munich date2
  muc_day_mid2 <- as.POSIXct(linear_time(date2 * 8 + 3.5, muc_illum))
  muc_noon2    <- as.POSIXct(linear_time(date2 * 2 + 1,   muc_ampm))
  expect_equal(as.numeric(muc_day_mid2), as.numeric(muc_noon2), tolerance = 300)
})
