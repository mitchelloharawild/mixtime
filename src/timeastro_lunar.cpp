#include "cpp11.hpp"
#include "timeastro.h"
#include <cmath>
#include <limits>

using namespace cpp11;

// ============================================================================
// Lunar calculations
// (Based on Jean Meeus "Astronomical Algorithms", chapters 47 & 49)
// ============================================================================

// JDE of new moon for lunation index k (Meeus ch.49)
static double jde_new_moon_k(double k) {
  double T  = k / 1236.85;
  double T2 = T * T;
  double T3 = T2 * T;
  double T4 = T3 * T;

  double JDE = 2451550.09766
             + SYNODIC_MONTH * k
             + 0.00015437    * T2
             - 0.000000150   * T3
             + 0.00000000073 * T4;

  double M  = (2.5534   + 29.10535670  * k - 0.0000014  * T2) * DEG2RAD;
  double Mp = (201.5643 + 385.81693528 * k + 0.0107582  * T2 + 0.00001238 * T3) * DEG2RAD;
  double F  = (160.7108 + 390.67050284 * k - 0.0016118  * T2 - 0.00000227 * T3) * DEG2RAD;
  double Om = (124.7746 -   1.56375588 * k + 0.0020672  * T2 + 0.00000215 * T3) * DEG2RAD;

  double corr = -0.40720 * std::sin(Mp)
                + 0.17241 * std::sin(M)
                + 0.01608 * std::sin(2.0 * Mp)
                + 0.01039 * std::sin(2.0 * F)
                + 0.00739 * std::sin(Mp - M)
                - 0.00514 * std::sin(Mp + M)
                + 0.00208 * std::sin(2.0 * M)
                - 0.00111 * std::sin(Mp - 2.0 * F)
                - 0.00057 * std::sin(Mp + 2.0 * F)
                + 0.00056 * std::sin(2.0 * Mp + M)
                - 0.00042 * std::sin(3.0 * Mp)
                + 0.00042 * std::sin(M  + 2.0 * F)
                + 0.00038 * std::sin(M  - 2.0 * F)
                - 0.00024 * std::sin(2.0 * Mp - M)
                - 0.00017 * std::sin(Om)
                - 0.00007 * std::sin(Mp + 2.0 * M)
                + 0.00004 * std::sin(2.0 * Mp - 2.0 * F)
                + 0.00004 * std::sin(3.0 * M)
                + 0.00003 * std::sin(Mp + M  - 2.0 * F)
                + 0.00003 * std::sin(2.0 * Mp + 2.0 * F)
                - 0.00003 * std::sin(Mp + M  + 2.0 * F)
                + 0.00003 * std::sin(Mp - M  + 2.0 * F)
                - 0.00002 * std::sin(Mp - M  - 2.0 * F)
                - 0.00002 * std::sin(3.0 * Mp + M)
                + 0.00002 * std::sin(4.0 * Mp);

  return JDE + corr;
}

static double new_moon_unix(double k) {
  return jd_to_unix(jde_new_moon_k(k));
}

// Lunation index of the new moon at or before a Unix time
static double lunation_index_before_or_at(double unix_time) {
  double k = std::floor((unix_to_jd(unix_time) - 2451550.09766) / SYNODIC_MONTH);
  if (new_moon_unix(k) > unix_time + 60.0) k -= 1.0;
  return k;
}

// Moon's phase elongation in degrees [0, 360): 0 deg = new, 180 deg = full.
// Shares sun_apparent_longitude() and unix_to_julian_centuries() with solar code.
// static double moon_phase_angle(double jd) {
//   double T1  = jd_to_julian_centuries(jd);
//   double T2 = T1 * T1;
//   double T3 = T2 * T1;
//   double T4 = T3 * T1;

//   // Moon's mean elements (Meeus eqs 47.1-47.5)
//   double Lp = 218.3164477 + 481267.88123421 * T1 - 0.0015786 * T2 + T3 / 538841.0   - T4 / 65194000.0;
//   double D  = 297.8501921 + 445267.1114034  * T1 - 0.0018819 * T2 + T3 / 545868.0   - T4 / 113065000.0;
//   double M  = 357.5291092 + 35999.0502909   * T1 - 0.0001536 * T2 + T3 / 24490000.0;
//   double Mp = 134.9633964 + 477198.8675055  * T1 + 0.0087414 * T2 + T3 / 69699.0    - T4 / 14712000.0;
//   double F  =  93.2720950 + 483202.0175233  * T1 - 0.0036539 * T2 - T3 / 3526000.0  + T4 / 863310000.0;

//   double D_r  = D  * DEG2RAD;
//   double M_r  = M  * DEG2RAD;
//   double Mp_r = Mp * DEG2RAD;
//   double F_r  = F  * DEG2RAD;

//   // Longitude perturbations (Meeus Table 47.A), units 1e-6 degrees
//   double dL = 6288774 * std::sin(Mp_r)
//             + 1274027 * std::sin(2.0*D_r - Mp_r)
//             +  658314 * std::sin(2.0*D_r)
//             +  213618 * std::sin(2.0*Mp_r)
//             -  185116 * std::sin(M_r)
//             -  114332 * std::sin(2.0*F_r)
//             +   58793 * std::sin(2.0*D_r - 2.0*Mp_r)
//             +   57066 * std::sin(2.0*D_r - M_r - Mp_r)
//             +   53322 * std::sin(2.0*D_r + Mp_r)
//             +   45758 * std::sin(2.0*D_r - M_r)
//             -   40923 * std::sin(M_r - Mp_r)
//             -   34720 * std::sin(D_r)
//             -   30383 * std::sin(M_r + Mp_r)
//             +   15327 * std::sin(2.0*D_r - 2.0*F_r)
//             -   12528 * std::sin(Mp_r + 2.0*F_r)
//             +   10980 * std::sin(Mp_r - 2.0*F_r)
//             +   10675 * std::sin(4.0*D_r - Mp_r)
//             +   10034 * std::sin(3.0*Mp_r)
//             +    8548 * std::sin(4.0*D_r - 2.0*Mp_r)
//             -    7888 * std::sin(2.0*D_r + M_r - Mp_r)
//             -    6766 * std::sin(2.0*D_r + M_r)
//             -    5163 * std::sin(D_r - Mp_r);

//   double lambda_moon = Lp + dL / 1000000.0;

//   // Reuse shared solar helper for Sun's apparent longitude
//   double lambda_sun = sun_apparent_longitude(T1);

//   double elongation = std::fmod(lambda_moon - lambda_sun, 360.0);
//   if (elongation < 0) elongation += 360.0;
//   return elongation;
// }

// ============================================================================
// Scalar lunation helpers (shared by exported functions)
// ============================================================================

// Continuous lunation count for a single Unix timestamp
static double lunation_count_scalar(double ut) {
  double k   = lunation_index_before_or_at(ut);
  double nm0 = new_moon_unix(k);
  double nm1 = new_moon_unix(k + 1.0);
  return k + (ut - nm0) / (nm1 - nm0);
}

// Unix timestamp from a continuous lunation count
static double utc_from_lunation_scalar(double lc) {
  double k    = std::floor(lc);
  double frac = lc - k;
  double nm0  = new_moon_unix(k);
  double nm1  = new_moon_unix(k + 1.0);
  return nm0 + frac * (nm1 - nm0);
}

// ============================================================================
// Exported R functions — lunar
// ============================================================================

// UTC Unix timestamps -> continuous lunation count since k=0 (J2000 new moon).
// Integer part = lunation index; fraction = phase [0, 1).
[[cpp11::register]]
doubles approx_lunations_from_utc(doubles unix_times) {
  int n = unix_times.size();
  writable::doubles result(n);
  for (int i = 0; i < n; i++) {
    double ut = unix_times[i];
    result[i] = std::isnan(ut) ? NA_REAL : lunation_count_scalar(ut);
  }
  return result;
}

// Continuous lunation counts -> UTC Unix timestamps.
[[cpp11::register]]
doubles approx_utc_from_lunations(doubles lunation_counts) {
  int n = lunation_counts.size();
  writable::doubles result(n);
  for (int i = 0; i < n; i++) {
    double lc = lunation_counts[i];
    result[i] = std::isnan(lc) ? NA_REAL : utc_from_lunation_scalar(lc);
  }
  return result;
}

// UTC Unix timestamps -> lunar phase fraction [0, 1).
// 0 = new moon, 0.25 ~= first quarter, 0.5 = full moon, 0.75 ~= last quarter.
[[cpp11::register]]
doubles approx_lunar_phase_from_utc(doubles unix_times) {
  int n = unix_times.size();
  writable::doubles result(n);
  for (int i = 0; i < n; i++) {
    double ut = unix_times[i];
    if (std::isnan(ut)) { result[i] = NA_REAL; continue; }
    double lc  = lunation_count_scalar(ut);
    result[i]  = lc - std::floor(lc);
  }
  return result;
}

// Continuous lunation counts (index + phase) -> UTC Unix timestamps.
[[cpp11::register]]
doubles approx_utc_from_lunar_phase(doubles lunation_counts) {
  return approx_utc_from_lunations(lunation_counts);
}
