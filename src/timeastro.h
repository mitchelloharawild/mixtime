#pragma once
#include <cmath>
#include <limits>

// ============================================================================
// Shared constants
// ============================================================================

// J2000 epoch = 2000-01-01 12:00:00 UTC as Unix timestamp
inline constexpr double J2000_UNIX         = 946728000.0;
inline constexpr double PI                 = 3.141592653589793;
inline constexpr double DEG2RAD            = PI / 180.0;
inline constexpr double RAD2DEG            = 180.0 / PI;
inline constexpr double SYNODIC_MONTH      = 29.530588861;  // mean synodic month, days

inline constexpr double JD_UNIX_EPOCH      = 2440587.5;     // JD at 1970-01-01 00:00:00 UTC
inline constexpr double JD_J2000           = 2451545.0;     // JD at J2000.0
inline constexpr double SECONDS_PER_DAY    = 86400.0;
inline constexpr double JULIAN_CENTURY     = 36525.0;
inline constexpr double MEAN_OBLIQUITY_J2000_DEG = 23.43928; // mean obliquity at J2000, degrees

// ============================================================================
// Shared time conversion helpers
// ============================================================================

// Unix timestamp → Julian Day Number
[[nodiscard]] constexpr double unix_to_jd(double unix_time) noexcept {
  return unix_time / SECONDS_PER_DAY + JD_UNIX_EPOCH;
}

// Julian Day Number → Unix timestamp
[[nodiscard]] constexpr double jd_to_unix(double jd) noexcept {
  return (jd - JD_UNIX_EPOCH) * SECONDS_PER_DAY;
}

// Julian centuries since J2000.0 from a Julian Day Number
[[nodiscard]] constexpr double jd_to_julian_centuries(double jd) noexcept {
  return (jd - JD_J2000) / JULIAN_CENTURY;
}

// Julian centuries since J2000.0 from a Unix timestamp
[[nodiscard]] constexpr double unix_to_julian_centuries(double unix_time) noexcept {
  return jd_to_julian_centuries(unix_to_jd(unix_time));
}

// ============================================================================
// Solar position helpers
// All accept T = Julian centuries since J2000.0
// ============================================================================

// Mean longitude of the Sun, degrees [0, 360)
[[nodiscard]] double sun_mean_longitude(double T);

// Mean anomaly of the Sun, degrees [0, 360)
[[nodiscard]] double sun_mean_anomaly(double T);

// Equation of center (true anomaly − mean anomaly), degrees
[[nodiscard]] double sun_equation_of_center(double M_deg, double T);

// Sun's ecliptic longitude (L0 + equation of center), degrees [0, 360)
[[nodiscard]] double sun_true_longitude(double L0, double M_deg, double T);

// Sun's declination from ecliptic longitude and T (obliquity varies with T),
// degrees [-23.44, +23.44]
[[nodiscard]] double sun_declination(double lambda_deg, double T);

// Equation of time (apparent − mean solar time), minutes
[[nodiscard]] double equation_of_time(double L0, double M_deg, double T);

// Sun's apparent longitude from T (used by lunar elongation)
[[nodiscard]] double sun_apparent_longitude(double T);
