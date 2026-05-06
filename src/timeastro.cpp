#include "timeastro.h"
#include <cmath>

// Implements the solar position helpers declared in timeastro.h.
// The time-conversion primitives (unix_to_jd, jd_to_unix,
// jd_to_julian_centuries, unix_to_julian_centuries) are constexpr and defined
// entirely in timeastro.h.  The functions here require <cmath> and are
// therefore not constexpr.

// ============================================================================
// Solar position helpers
// All accept T = Julian centuries since J2000.0
// ============================================================================

// Mean longitude of the Sun, degrees [0, 360)
double sun_mean_longitude(double T) {
  double L0 = 280.46646 + T * (36000.76983 + T * 0.0003032);
  L0 = std::fmod(L0, 360.0);
  if (L0 < 0.0) L0 += 360.0;
  return L0;
}

// Mean anomaly of the Sun, degrees [0, 360)
double sun_mean_anomaly(double T) {
  double M = 357.52911 + T * (35999.05029 - T * 0.0001537);
  M = std::fmod(M, 360.0);
  if (M < 0.0) M += 360.0;
  return M;
}

// Equation of center (true anomaly - mean anomaly), degrees
double sun_equation_of_center(double M_deg, double T) {
  double M = M_deg * DEG2RAD;
  return (1.914602 + T * (-0.004817 - T * 0.000014)) * std::sin(M)
       + (0.019993 - T *  0.000101)                   * std::sin(2.0 * M)
       +  0.000289                                     * std::sin(3.0 * M);
}

// Sun's ecliptic longitude (L0 + equation of center), degrees [0, 360)
double sun_true_longitude(double L0, double M_deg, double T) {
  double L = L0 + sun_equation_of_center(M_deg, T);
  L = std::fmod(L, 360.0);
  if (L < 0.0) L += 360.0;
  return L;
}

// Sun's declination from ecliptic longitude and T, degrees [-23.44, +23.44]
// Obliquity is evaluated at T rather than fixed at J2000 to maintain accuracy
// over multi-decade spans (~0.47° drift per century).
double sun_declination(double lambda_deg, double T) {
  double epsilon = (MEAN_OBLIQUITY_J2000_DEG - T * 0.0130042) * DEG2RAD;
  double lambda  = lambda_deg * DEG2RAD;
  return std::asin(std::sin(epsilon) * std::sin(lambda)) * RAD2DEG;
}

// Equation of time (apparent - mean solar time), minutes
double equation_of_time(double L0, double M_deg, double T) {
  double e       = 0.016708634 + T * (-0.000042037 - T * 0.0000001267);
  double epsilon = (23.43929111 - T * 0.0130042) * DEG2RAD;

  double y   = std::tan(epsilon / 2.0);
  y          = y * y;
  double L0r = L0    * DEG2RAD;
  double Mr  = M_deg * DEG2RAD;

  double EoT = y             * std::sin(2.0 * L0r)
             - 2.0 * e       * std::sin(Mr)
             + 4.0 * e * y   * std::sin(Mr) * std::cos(2.0 * L0r)
             - 0.5 * y * y   * std::sin(4.0 * L0r)
             - 1.25 * e * e  * std::sin(2.0 * Mr);

  return EoT * RAD2DEG * 4.0; // radians → minutes (4 min per degree)
}

// Sun's apparent longitude from T (used by lunar elongation)
double sun_apparent_longitude(double T) {
  double L0 = sun_mean_longitude(T);
  double M  = sun_mean_anomaly(T);
  return sun_true_longitude(L0, M, T);
}
