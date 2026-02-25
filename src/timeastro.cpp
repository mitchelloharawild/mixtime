#include "cpp11.hpp"
#include <cmath>

using namespace cpp11;

// J2000 epoch = 2000-01-01 12:00:00 UTC as Unix timestamp
static const double J2000_UNIX = 946728000.0;
static const double PI = 3.141592653589793;
static const double DEG2RAD = PI / 180.0;
static const double RAD2DEG = 180.0 / PI;

// ============================================================================
// Fast approximation for solar events without astronomy.c library calls
// ============================================================================

// Calculate mean longitude of the Sun (simplified from astronomy.c)
// Returns degrees [0, 360)
static double sun_mean_longitude(double T) {
  // T is Julian centuries since J2000 epoch
  double L0 = 280.46646 + 36000.76983 * T + 0.0003032 * std::pow(T, 2);
  L0 = std::fmod(L0, 360.0);
  if (L0 < 0) L0 += 360.0;
  return L0;
}

// Calculate mean anomaly of the Sun
// Returns degrees
static double sun_mean_anomaly(double T) {
  // T is Julian centuries since J2000 epoch
  double M = 357.52911 + 35999.05029 * T - 0.0001537 * std::pow(T, 2);
  return M;
}

// Calculate equation of center (Sun's true anomaly - mean anomaly)
// Returns degrees
static double sun_equation_of_center(double M_deg, double T) {
  // T is Julian centuries since J2000 epoch
  double M = M_deg * DEG2RAD;
  double C = (1.914602 - 0.004817 * T - 0.000014 * std::pow(T, 2)) * std::sin(M)
           + (0.019993 - 0.000101 * T) * std::sin(2 * M)
           + 0.000289 * std::sin(3 * M);
  return C;
}

// Calculate Sun's ecliptic longitude
// Returns degrees [0, 360)
static double sun_true_longitude(double L0, double M, double T) {
  double C = sun_equation_of_center(M, T);
  double L = L0 + C;
  L = std::fmod(L, 360.0);
  if (L < 0) L += 360.0;
  return L;
}

// Calculate Sun's declination from ecliptic longitude
// Returns degrees [-23.44, +23.44]
static double sun_declination(double lambda_deg) {
  // Obliquity of ecliptic (mean, J2000)
  double epsilon = 23.43928 * DEG2RAD;
  double lambda = lambda_deg * DEG2RAD;
  double dec = std::asin(std::sin(epsilon) * std::sin(lambda));
  return dec * RAD2DEG;
}

// Calculate equation of time (apparent solar time - mean solar time)
// Returns minutes
static double equation_of_time(double L0, double M, double T) {
  double e = 0.016708634 - 0.000042037 * T - 0.0000001267 * std::pow(T, 2); // eccentricity
  double epsilon = (23.43929111 - 0.0130042 * T) * DEG2RAD; // obliquity with variation
  
  double y = std::tan(epsilon / 2.0);
  y = y * y;
  
  double L0_rad = L0 * DEG2RAD;
  double M_rad = M * DEG2RAD;
  
  double EoT = y * std::sin(2.0 * L0_rad)
             - 2.0 * e * std::sin(M_rad)
             + 4.0 * e * y * std::sin(M_rad) * std::cos(2.0 * L0_rad)
             - 0.5 * y * y * std::sin(4.0 * L0_rad)
             - 1.25 * e * e * std::sin(2.0 * M_rad);
  
  return EoT * RAD2DEG * 4.0; // convert radians to minutes (4 min per degree)
}

// Calculate hour angle for sunrise/sunset
// lat_deg: observer latitude in degrees
// dec_deg: Sun's declination in degrees
// alt_deg: altitude threshold (default -0.833 for standard refraction + solar radius)
// Returns hour angle in hours, or NaN if no rise/set (polar day/night)
static double hour_angle_rise_set(double lat_deg, double dec_deg, double alt_deg = -0.833) {
  double lat = lat_deg * DEG2RAD;
  double dec = dec_deg * DEG2RAD;
  double alt = alt_deg * DEG2RAD;
  
  double cos_ha = (std::sin(alt) - std::sin(lat) * std::sin(dec)) / (std::cos(lat) * std::cos(dec));
  
  // Check for polar day/night
  if (cos_ha > 1.0 || cos_ha < -1.0) {
    return std::numeric_limits<double>::quiet_NaN();
  }
  
  double ha_rad = std::acos(cos_ha);
  return ha_rad * 12.0 / PI; // convert radians to hours
}

// Helper structure to hold solar calculations for a given day
struct SolarDay {
  double day_utc;
  double noon_hour;
  double ha;
  bool valid;
  
  SolarDay(double day, double lat_deg, double lon_deg, double alt_deg) {
    day_utc = day;
    valid = false;
    
    double t = (day_utc + 0.5) - J2000_UNIX / 86400.0;
    double T = t / 36525.0;
    double L0 = sun_mean_longitude(T);
    double M = sun_mean_anomaly(T);
    double lambda = sun_true_longitude(L0, M, T);
    double dec = sun_declination(lambda);
    double eot = equation_of_time(L0, M, T);
    
    noon_hour = 12.0 - eot / 60.0 - lon_deg / 15.0;
    ha = hour_angle_rise_set(lat_deg, dec, alt_deg);
    
    valid = !std::isnan(ha) && !std::isnan(noon_hour);
  }
  
  double sunrise_unix() const {
    return day_utc * 86400.0 + (noon_hour - ha) * 3600.0;
  }
  
  double sunset_unix() const {
    return day_utc * 86400.0 + (noon_hour + ha) * 3600.0;
  }
  
  double noon_unix() const {
    return day_utc * 86400.0 + noon_hour * 3600.0;
  }
  
  double midnight_unix() const {
    double midnight_hour = noon_hour - 12.0;
    if (midnight_hour < 0) {
      midnight_hour += 24.0;
    }
    return day_utc * 86400.0 + midnight_hour * 3600.0;
  }
};

// Generic function to convert UTC to event counts
template<typename EventFunc>
doubles event_counts_from_utc(doubles unix_times,
                               double lat_deg,
                               double lon_deg,
                               double alt_deg,
                               EventFunc get_event_time) {
  int n = unix_times.size();
  writable::doubles result(n);

  for (int i = 0; i < n; i++) {
    double ut = unix_times[i];
    if (std::isnan(ut)) {
      result[i] = NA_REAL;
      continue;
    }
    
    double day_utc = std::floor(ut / 86400.0);
    
    SolarDay today(day_utc, lat_deg, lon_deg, alt_deg);
    if (!today.valid) {
      result[i] = NA_REAL;
      continue;
    }
    
    double event_unix = get_event_time(today);
    double days_since_j2000 = (event_unix - J2000_UNIX) / 86400.0;
    double complete_events = std::floor(days_since_j2000);
    
    double fraction;
    if (ut < event_unix) {
      SolarDay prev(day_utc - 1.0, lat_deg, lon_deg, alt_deg);
      if (!prev.valid) {
        result[i] = NA_REAL;
        continue;
      }
      double prev_event_unix = get_event_time(prev);
      fraction = (ut - prev_event_unix) / (event_unix - prev_event_unix);
    } else {
      SolarDay next(day_utc + 1.0, lat_deg, lon_deg, alt_deg);
      if (!next.valid) {
        result[i] = NA_REAL;
        continue;
      }
      double next_event_unix = get_event_time(next);
      fraction = (ut - event_unix) / (next_event_unix - event_unix);
      complete_events += 1.0;
    }
    
    result[i] = complete_events + fraction;
  }

  return result;
}

// Generic function to convert event counts to UTC
template<typename EventFunc>
doubles utc_from_event_counts(doubles event_counts,
                               double lat_deg,
                               double lon_deg,
                               double alt_deg,
                               EventFunc get_event_time) {
  int n = event_counts.size();
  writable::doubles result(n);

  for (int i = 0; i < n; i++) {
    double count = event_counts[i];
    if (std::isnan(count)) {
      result[i] = NA_REAL;
      continue;
    }
    
    double complete = std::floor(count);
    double fraction = count - complete;
    double day_utc = std::floor(J2000_UNIX / 86400.0 + complete);
    
    SolarDay today(day_utc, lat_deg, lon_deg, alt_deg);
    if (!today.valid) {
      result[i] = NA_REAL;
      continue;
    }
    
    SolarDay next(day_utc + 1.0, lat_deg, lon_deg, alt_deg);
    if (!next.valid) {
      result[i] = NA_REAL;
      continue;
    }
    
    double event_unix = get_event_time(today);
    double next_event_unix = get_event_time(next);
    
    result[i] = event_unix + fraction * (next_event_unix - event_unix);
  }

  return result;
}

// ============================================================================
// Exported R functions
// ============================================================================


// Exported functions for sunrise
[[cpp11::register]]
doubles approx_sunrises_from_utc(doubles unix_times,
                                  double lat_deg,
                                  double lon_deg,
                                  double alt_deg = -0.833) {
  return event_counts_from_utc(unix_times, lat_deg, lon_deg, alt_deg,
                               [](const SolarDay& day) { return day.sunrise_unix(); });
}

[[cpp11::register]]
doubles approx_utc_from_sunrises(doubles sunrise_counts,
                                  double lat_deg,
                                  double lon_deg,
                                  double alt_deg = -0.833) {
  return utc_from_event_counts(sunrise_counts, lat_deg, lon_deg, alt_deg,
                               [](const SolarDay& day) { return day.sunrise_unix(); });
}

// Exported functions for sunset
[[cpp11::register]]
doubles approx_sunsets_from_utc(doubles unix_times,
                                 double lat_deg,
                                 double lon_deg,
                                 double alt_deg = -0.833) {
  return event_counts_from_utc(unix_times, lat_deg, lon_deg, alt_deg,
                               [](const SolarDay& day) { return day.sunset_unix(); });
}

[[cpp11::register]]
doubles approx_utc_from_sunsets(doubles sunset_counts,
                                 double lat_deg,
                                 double lon_deg,
                                 double alt_deg = -0.833) {
  return utc_from_event_counts(sunset_counts, lat_deg, lon_deg, alt_deg,
                               [](const SolarDay& day) { return day.sunset_unix(); });
}

// Exported functions for solar noon
[[cpp11::register]]
doubles approx_noons_from_utc(doubles unix_times,
                               double lat_deg,
                               double lon_deg) {
  return event_counts_from_utc(unix_times, lat_deg, lon_deg, 0.0,
                               [](const SolarDay& day) { return day.noon_unix(); });
}

[[cpp11::register]]
doubles approx_utc_from_noons(doubles noon_counts,
                               double lat_deg,
                               double lon_deg) {
  return utc_from_event_counts(noon_counts, lat_deg, lon_deg, 0.0,
                               [](const SolarDay& day) { return day.noon_unix(); });
}

// Exported functions for solar midnight
[[cpp11::register]]
doubles approx_midnights_from_utc(doubles unix_times,
                                   double lat_deg,
                                   double lon_deg) {
  return event_counts_from_utc(unix_times, lat_deg, lon_deg, 0.0,
                               [](const SolarDay& day) { return day.midnight_unix(); });
}

[[cpp11::register]]
doubles approx_utc_from_midnights(doubles midnight_counts,
                                   double lat_deg,
                                   double lon_deg) {
  return utc_from_event_counts(midnight_counts, lat_deg, lon_deg, 0.0,
                               [](const SolarDay& day) { return day.midnight_unix(); });
}

// Exported functions for dawn (civil twilight)
[[cpp11::register]]
doubles approx_dawns_from_utc(doubles unix_times,
                               double lat_deg,
                               double lon_deg,
                               double alt_deg = -6.0) {
  return event_counts_from_utc(unix_times, lat_deg, lon_deg, alt_deg,
                               [](const SolarDay& day) { return day.sunrise_unix(); });
}

[[cpp11::register]]
doubles approx_utc_from_dawns(doubles dawn_counts,
                               double lat_deg,
                               double lon_deg,
                               double alt_deg = -6.0) {
  return utc_from_event_counts(dawn_counts, lat_deg, lon_deg, alt_deg,
                               [](const SolarDay& day) { return day.sunrise_unix(); });
}

// Exported functions for dusk (civil twilight)
[[cpp11::register]]
doubles approx_dusks_from_utc(doubles unix_times,
                               double lat_deg,
                               double lon_deg,
                               double alt_deg = -6.0) {
  return event_counts_from_utc(unix_times, lat_deg, lon_deg, alt_deg,
                               [](const SolarDay& day) { return day.sunset_unix(); });
}

[[cpp11::register]]
doubles approx_utc_from_dusks(doubles dusk_counts,
                               double lat_deg,
                               double lon_deg,
                               double alt_deg = -6.0) {
  return utc_from_event_counts(dusk_counts, lat_deg, lon_deg, alt_deg,
                               [](const SolarDay& day) { return day.sunset_unix(); });
}



