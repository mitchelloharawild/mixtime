#include "cpp11.hpp"
#include <cmath>

using namespace cpp11;

// ============================================================================
// Shared constants
// ============================================================================

// J2000 epoch = 2000-01-01 12:00:00 UTC as Unix timestamp
static const double J2000_UNIX     = 946728000.0;
static const double PI             = 3.141592653589793;
static const double DEG2RAD        = PI / 180.0;
static const double RAD2DEG        = 180.0 / PI;
static const double SYNODIC_MONTH  = 29.530588861; // mean synodic month, days

// ============================================================================
// Shared time conversion helpers
// ============================================================================

// Unix timestamp → Julian Day Number
static double unix_to_jd(double unix_time) {
  return unix_time / 86400.0 + 2440587.5;
}

// Julian Day Number → Unix timestamp
static double jd_to_unix(double jd) {
  return (jd - 2440587.5) * 86400.0;
}

// Julian centuries since J2000.0 from a Julian Day Number
static double jd_to_julian_centuries(double jd) {
  return (jd - 2451545.0) / 36525.0;
}

// Julian centuries since J2000.0 from a Unix timestamp
static double unix_to_julian_centuries(double unix_time) {
  return jd_to_julian_centuries(unix_to_jd(unix_time));
}

// ============================================================================
// Solar position helpers
// All accept T = Julian centuries since J2000.0
// ============================================================================

// Mean longitude of the Sun, degrees [0, 360)
static double sun_mean_longitude(double T) {
  double L0 = 280.46646 + 36000.76983 * T + 0.0003032 * T * T;
  L0 = std::fmod(L0, 360.0);
  if (L0 < 0) L0 += 360.0;
  return L0;
}

// Mean anomaly of the Sun, degrees
static double sun_mean_anomaly(double T) {
  return 357.52911 + 35999.05029 * T - 0.0001537 * T * T;
}

// Equation of center (true anomaly − mean anomaly), degrees
static double sun_equation_of_center(double M_deg, double T) {
  double M = M_deg * DEG2RAD;
  return (1.914602 - 0.004817 * T - 0.000014 * T * T) * std::sin(M)
       + (0.019993 - 0.000101 * T)                     * std::sin(2.0 * M)
       +  0.000289                                      * std::sin(3.0 * M);
}

// Sun's ecliptic longitude (L0 + equation of center), degrees [0, 360)
static double sun_true_longitude(double L0, double M_deg, double T) {
  double L = L0 + sun_equation_of_center(M_deg, T);
  L = std::fmod(L, 360.0);
  if (L < 0) L += 360.0;
  return L;
}

// Sun's declination from ecliptic longitude, degrees [-23.44, +23.44]
static double sun_declination(double lambda_deg) {
  static const double epsilon = 23.43928 * DEG2RAD; // mean obliquity at J2000
  double lambda = lambda_deg * DEG2RAD;
  return std::asin(std::sin(epsilon) * std::sin(lambda)) * RAD2DEG;
}

// Equation of time (apparent − mean solar time), minutes
static double equation_of_time(double L0, double M_deg, double T) {
  double e       = 0.016708634 - 0.000042037 * T - 0.0000001267 * T * T;
  double epsilon = (23.43929111 - 0.0130042 * T) * DEG2RAD;

  double y     = std::tan(epsilon / 2.0);
  y            = y * y;
  double L0r   = L0    * DEG2RAD;
  double Mr    = M_deg * DEG2RAD;

  double EoT = y         * std::sin(2.0 * L0r)
             - 2.0 * e   * std::sin(Mr)
             + 4.0 * e * y * std::sin(Mr) * std::cos(2.0 * L0r)
             - 0.5  * y * y * std::sin(4.0 * L0r)
             - 1.25 * e * e * std::sin(2.0 * Mr);

  return EoT * RAD2DEG * 4.0; // radians → minutes (4 min per degree)
}

// Sun's apparent longitude from T (used by lunar elongation)
static double sun_apparent_longitude(double T) {
  double L0  = sun_mean_longitude(T);
  double M   = sun_mean_anomaly(T);
  return sun_true_longitude(L0, M, T);
}

// ============================================================================
// Solar event geometry
// ============================================================================

// Hour angle for rise/set at a given altitude threshold, returned in hours.
// Returns NaN for polar day/night.
static double hour_angle_rise_set(double lat_deg, double dec_deg,
                                   double alt_deg = -0.833) {
  double lat = lat_deg * DEG2RAD;
  double dec = dec_deg * DEG2RAD;
  double alt = alt_deg * DEG2RAD;

  double cos_ha = (std::sin(alt) - std::sin(lat) * std::sin(dec))
                / (std::cos(lat) * std::cos(dec));

  if (cos_ha > 1.0 || cos_ha < -1.0)
    return std::numeric_limits<double>::quiet_NaN();

  return std::acos(cos_ha) * 12.0 / PI; // radians → hours
}

// ============================================================================
// SolarDay: pre-computed solar geometry for one UTC day
// ============================================================================

struct SolarDay {
  double day_utc;   // whole days since Unix epoch
  double noon_hour; // solar noon in fractional hours UTC
  double ha;        // half-day hour angle (hours)
  bool   valid;

  SolarDay(double day, double lat_deg, double lon_deg, double alt_deg) {
    day_utc = day;
    valid   = false;

    // Evaluate at solar noon of the day (day + 0.5 in days since Unix epoch)
    double jd = unix_to_jd((day_utc + 0.5) * 86400.0);
    double T  = jd_to_julian_centuries(jd);

    double L0     = sun_mean_longitude(T);
    double M      = sun_mean_anomaly(T);
    double lambda  = sun_true_longitude(L0, M, T);
    double dec    = sun_declination(lambda);
    double eot    = equation_of_time(L0, M, T);

    noon_hour = 12.0 - eot / 60.0 - lon_deg / 15.0;
    ha        = hour_angle_rise_set(lat_deg, dec, alt_deg);

    valid = !std::isnan(ha) && !std::isnan(noon_hour);
  }

  double sunrise_unix() const { return day_utc * 86400.0 + (noon_hour - ha) * 3600.0; }
  double sunset_unix()  const { return day_utc * 86400.0 + (noon_hour + ha) * 3600.0; }
  double noon_unix()    const { return day_utc * 86400.0 +  noon_hour        * 3600.0; }

  double midnight_unix() const {
    double mh = noon_hour - 12.0;
    if (mh < 0) mh += 24.0;
    return day_utc * 86400.0 + mh * 3600.0;
  }
};

// ============================================================================
// Generic solar event count conversion templates
// ============================================================================

template<typename EventFunc>
doubles event_counts_from_utc(doubles unix_times,
                               double lat_deg, double lon_deg, double alt_deg,
                               EventFunc get_event_time) {
  int n = unix_times.size();
  writable::doubles result(n);

  for (int i = 0; i < n; i++) {
    double ut = unix_times[i];
    if (std::isnan(ut)) { result[i] = NA_REAL; continue; }

    double day_utc = std::floor(ut / 86400.0);

    SolarDay today(day_utc, lat_deg, lon_deg, alt_deg);
    if (!today.valid) { result[i] = NA_REAL; continue; }

    double event_unix     = get_event_time(today);
    double days_since_j2000 = (event_unix - J2000_UNIX) / 86400.0;
    double complete_events  = std::floor(days_since_j2000);

    double fraction;
    if (ut < event_unix) {
      SolarDay prev(day_utc - 1.0, lat_deg, lon_deg, alt_deg);
      if (!prev.valid) { result[i] = NA_REAL; continue; }
      double prev_event = get_event_time(prev);
      fraction = (ut - prev_event) / (event_unix - prev_event);
    } else {
      SolarDay next(day_utc + 1.0, lat_deg, lon_deg, alt_deg);
      if (!next.valid) { result[i] = NA_REAL; continue; }
      double next_event = get_event_time(next);
      fraction = (ut - event_unix) / (next_event - event_unix);
      complete_events += 1.0;
    }

    result[i] = complete_events + fraction;
  }
  return result;
}

template<typename EventFunc>
doubles utc_from_event_counts(doubles event_counts,
                               double lat_deg, double lon_deg, double alt_deg,
                               EventFunc get_event_time) {
  int n = event_counts.size();
  writable::doubles result(n);

  for (int i = 0; i < n; i++) {
    double count = event_counts[i];
    if (std::isnan(count)) { result[i] = NA_REAL; continue; }

    double complete = std::floor(count);
    double fraction = count - complete;
    double day_utc  = std::floor(J2000_UNIX / 86400.0 + complete);

    SolarDay today(day_utc,       lat_deg, lon_deg, alt_deg);
    SolarDay next (day_utc + 1.0, lat_deg, lon_deg, alt_deg);
    if (!today.valid || !next.valid) { result[i] = NA_REAL; continue; }

    double event_unix      = get_event_time(today);
    double next_event_unix = get_event_time(next);

    result[i] = event_unix + fraction * (next_event_unix - event_unix);
  }
  return result;
}

// ============================================================================
// Lunar calculations
// (Based on Jean Meeus "Astronomical Algorithms", chapters 47 & 49)
// ============================================================================

// JDE of new moon for lunation index k (Meeus ch.49)
static double jde_new_moon_k(double k) {
  double T   = k / 1236.85;
  double JDE = 2451550.09766
             + SYNODIC_MONTH * k
             + 0.00015437     * T * T
             - 0.000000150    * T * T * T
             + 0.00000000073  * T * T * T * T;

  double M  = (2.5534   + 29.10535670  * k - 0.0000014  * T * T) * DEG2RAD;
  double Mp = (201.5643 + 385.81693528 * k + 0.0107582  * T * T
               + 0.00001238 * T * T * T) * DEG2RAD;
  double F  = (160.7108 + 390.67050284 * k - 0.0016118  * T * T
               - 0.00000227 * T * T * T) * DEG2RAD;
  double Om = (124.7746 - 1.56375588   * k + 0.0020672  * T * T
               + 0.00000215 * T * T * T) * DEG2RAD;

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

// Moon's phase elongation in degrees [0, 360): 0° = new, 180° = full.
// Shares sun_apparent_longitude() and unix_to_julian_centuries() with solar code.
static double moon_phase_angle(double jd) {
  double T = jd_to_julian_centuries(jd);

  // Moon's mean elements (Meeus eqs 47.1–47.5)
  double Lp = 218.3164477 + 481267.88123421 * T - 0.0015786 * T * T
            + T * T * T / 538841.0 - T * T * T * T / 65194000.0;
  double D  = 297.8501921 + 445267.1114034  * T - 0.0018819 * T * T
            + T * T * T / 545868.0 - T * T * T * T / 113065000.0;
  double M  = 357.5291092 + 35999.0502909   * T - 0.0001536 * T * T
            + T * T * T / 24490000.0;
  double Mp = 134.9633964 + 477198.8675055  * T + 0.0087414 * T * T
            + T * T * T / 69699.0 - T * T * T * T / 14712000.0;
  double F  =  93.2720950 + 483202.0175233  * T - 0.0036539 * T * T
            - T * T * T / 3526000.0 + T * T * T * T / 863310000.0;

  double D_r  = D  * DEG2RAD;
  double M_r  = M  * DEG2RAD;
  double Mp_r = Mp * DEG2RAD;
  double F_r  = F  * DEG2RAD;

  // Longitude perturbations (Meeus Table 47.A), units 1e-6 degrees
  double dL = 6288774 * std::sin(Mp_r)
            + 1274027 * std::sin(2.0*D_r - Mp_r)
            +  658314 * std::sin(2.0*D_r)
            +  213618 * std::sin(2.0*Mp_r)
            -  185116 * std::sin(M_r)
            -  114332 * std::sin(2.0*F_r)
            +   58793 * std::sin(2.0*D_r - 2.0*Mp_r)
            +   57066 * std::sin(2.0*D_r - M_r - Mp_r)
            +   53322 * std::sin(2.0*D_r + Mp_r)
            +   45758 * std::sin(2.0*D_r - M_r)
            -   40923 * std::sin(M_r - Mp_r)
            -   34720 * std::sin(D_r)
            -   30383 * std::sin(M_r + Mp_r)
            +   15327 * std::sin(2.0*D_r - 2.0*F_r)
            -   12528 * std::sin(Mp_r + 2.0*F_r)
            +   10980 * std::sin(Mp_r - 2.0*F_r)
            +   10675 * std::sin(4.0*D_r - Mp_r)
            +   10034 * std::sin(3.0*Mp_r)
            +    8548 * std::sin(4.0*D_r - 2.0*Mp_r)
            -    7888 * std::sin(2.0*D_r + M_r - Mp_r)
            -    6766 * std::sin(2.0*D_r + M_r)
            -    5163 * std::sin(D_r - Mp_r);

  double lambda_moon = Lp + dL / 1000000.0;

  // Reuse shared solar helper for Sun's apparent longitude
  double lambda_sun = sun_apparent_longitude(T);

  double elongation = std::fmod(lambda_moon - lambda_sun, 360.0);
  if (elongation < 0) elongation += 360.0;
  return elongation;
}

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

// UTC Unix timestamps → continuous lunation count since k=0 (J2000 new moon).
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

// Continuous lunation counts → UTC Unix timestamps.
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

// UTC Unix timestamps → lunar phase fraction [0, 1).
// 0 = new moon, 0.25 ≈ first quarter, 0.5 = full moon, 0.75 ≈ last quarter.
[[cpp11::register]]
doubles approx_lunar_phase_from_utc(doubles unix_times) {
  int n = unix_times.size();
  writable::doubles result(n);
  for (int i = 0; i < n; i++) {
    double ut = unix_times[i];
    if (std::isnan(ut)) { result[i] = NA_REAL; continue; }
    result[i] = lunation_count_scalar(ut) - std::floor(lunation_count_scalar(ut));
  }
  return result;
}

// Continuous lunation counts (index + phase) → UTC Unix timestamps.
[[cpp11::register]]
doubles approx_utc_from_lunar_phase(doubles lunation_counts) {
  return approx_utc_from_lunations(lunation_counts);
}

// ============================================================================
// Exported R functions — solar
// ============================================================================

[[cpp11::register]]
doubles approx_sunrises_from_utc(doubles unix_times,
                                  double lat_deg, double lon_deg,
                                  double alt_deg = -0.833) {
  return event_counts_from_utc(unix_times, lat_deg, lon_deg, alt_deg,
    [](const SolarDay& d) { return d.sunrise_unix(); });
}

[[cpp11::register]]
doubles approx_utc_from_sunrises(doubles sunrise_counts,
                                  double lat_deg, double lon_deg,
                                  double alt_deg = -0.833) {
  return utc_from_event_counts(sunrise_counts, lat_deg, lon_deg, alt_deg,
    [](const SolarDay& d) { return d.sunrise_unix(); });
}

[[cpp11::register]]
doubles approx_sunsets_from_utc(doubles unix_times,
                                 double lat_deg, double lon_deg,
                                 double alt_deg = -0.833) {
  return event_counts_from_utc(unix_times, lat_deg, lon_deg, alt_deg,
    [](const SolarDay& d) { return d.sunset_unix(); });
}

[[cpp11::register]]
doubles approx_utc_from_sunsets(doubles sunset_counts,
                                 double lat_deg, double lon_deg,
                                 double alt_deg = -0.833) {
  return utc_from_event_counts(sunset_counts, lat_deg, lon_deg, alt_deg,
    [](const SolarDay& d) { return d.sunset_unix(); });
}

[[cpp11::register]]
doubles approx_noons_from_utc(doubles unix_times,
                               double lat_deg, double lon_deg) {
  return event_counts_from_utc(unix_times, lat_deg, lon_deg, 0.0,
    [](const SolarDay& d) { return d.noon_unix(); });
}

[[cpp11::register]]
doubles approx_utc_from_noons(doubles noon_counts,
                               double lat_deg, double lon_deg) {
  return utc_from_event_counts(noon_counts, lat_deg, lon_deg, 0.0,
    [](const SolarDay& d) { return d.noon_unix(); });
}

[[cpp11::register]]
doubles approx_midnights_from_utc(doubles unix_times,
                                   double lat_deg, double lon_deg) {
  return event_counts_from_utc(unix_times, lat_deg, lon_deg, 0.0,
    [](const SolarDay& d) { return d.midnight_unix(); });
}

[[cpp11::register]]
doubles approx_utc_from_midnights(doubles midnight_counts,
                                   double lat_deg, double lon_deg) {
  return utc_from_event_counts(midnight_counts, lat_deg, lon_deg, 0.0,
    [](const SolarDay& d) { return d.midnight_unix(); });
}

[[cpp11::register]]
doubles approx_dawns_from_utc(doubles unix_times,
                               double lat_deg, double lon_deg,
                               double alt_deg = -6.0) {
  return event_counts_from_utc(unix_times, lat_deg, lon_deg, alt_deg,
    [](const SolarDay& d) { return d.sunrise_unix(); });
}

[[cpp11::register]]
doubles approx_utc_from_dawns(doubles dawn_counts,
                               double lat_deg, double lon_deg,
                               double alt_deg = -6.0) {
  return utc_from_event_counts(dawn_counts, lat_deg, lon_deg, alt_deg,
    [](const SolarDay& d) { return d.sunrise_unix(); });
}

[[cpp11::register]]
doubles approx_dusks_from_utc(doubles unix_times,
                               double lat_deg, double lon_deg,
                               double alt_deg = -6.0) {
  return event_counts_from_utc(unix_times, lat_deg, lon_deg, alt_deg,
    [](const SolarDay& d) { return d.sunset_unix(); });
}

[[cpp11::register]]
doubles approx_utc_from_dusks(doubles dusk_counts,
                               double lat_deg, double lon_deg,
                               double alt_deg = -6.0) {
  return utc_from_event_counts(dusk_counts, lat_deg, lon_deg, alt_deg,
    [](const SolarDay& d) { return d.sunset_unix(); });
}

