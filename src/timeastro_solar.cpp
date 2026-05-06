#include "cpp11.hpp"
#include "timeastro.h"
#include <cmath>
#include <limits>
#include <unordered_map>

using namespace cpp11;

// ============================================================================
// Solar event geometry
// ============================================================================


// ============================================================================
// SolarGeometry: sun position for one UTC day, independent of altitude.
//
// Separating geometry from altitude allows the expensive trig (JD, Julian
// centuries, sun longitude, declination, equation of time) to be computed
// once per UTC day and reused for all altitude thresholds.
// ============================================================================
// Intermediate sun-position values computed once per UTC day and consumed by
// the SolarGeometry constructor.  Keeping them in a separate struct avoids
// repeating the JD / Julian-century / trig pipeline inside SolarGeometry.
struct SunPos {
  double dec_deg;
  double noon_hour;

  static SunPos compute(double day_utc, double lon_deg) {
    double jd     = unix_to_jd((day_utc + 0.5) * 86400.0);
    double T      = jd_to_julian_centuries(jd);
    double L0     = sun_mean_longitude(T);
    double M      = sun_mean_anomaly(T);
    double lambda = sun_true_longitude(L0, M, T);
    double eot    = equation_of_time(L0, M, T);
    return { sun_declination(lambda, T),
             12.0 - eot / 60.0 - lon_deg / 15.0 };
  }
};

struct SolarGeometry {
  double day_utc;   // whole days since Unix epoch
  double lat_rad;
  double sin_lat;   // precomputed for ha evaluation
  double cos_lat;
  double sin_dec;   // precomputed for ha evaluation
  double cos_dec;
  double noon_hour; // solar noon in fractional hours UTC
  double dec_deg;   // solar declination (degrees)
  double alt_deg;   // observer elevation correction (degrees)
  bool   valid;

  SolarGeometry() : day_utc(0), lat_rad(0), sin_lat(0), cos_lat(0),
                    sin_dec(0), cos_dec(0),
                    noon_hour(0), dec_deg(0), valid(false) {}

  SolarGeometry(double day, double lat_deg, double lon_deg)
    : day_utc(day),
      lat_rad(lat_deg * DEG2RAD),
      sin_lat(std::sin(lat_deg * DEG2RAD)),
      cos_lat(std::cos(lat_deg * DEG2RAD))
  {
    SunPos sp = SunPos::compute(day, lon_deg);
    dec_deg   = sp.dec_deg;
    noon_hour = sp.noon_hour;
    double dec_rad = dec_deg * DEG2RAD;
    sin_dec   = std::sin(dec_rad);
    cos_dec   = std::cos(dec_rad);
    valid     = !std::isnan(noon_hour) && !std::isnan(dec_deg);
  }

  // Hour angle (hours) for a given altitude threshold; NaN for polar day/night.
  double ha(double alt_deg) const {
    double alt    = alt_deg * DEG2RAD;
    double cos_ha = (std::sin(alt) - sin_lat * sin_dec)
                  / (cos_lat * cos_dec);
    if (cos_ha > 1.0 || cos_ha < -1.0)
      return std::numeric_limits<double>::quiet_NaN();
    return std::acos(cos_ha) * 12.0 / PI; // radians -> hours
  }

  // Rise (morning=true) or set time as Unix timestamp for given altitude threshold.
  double event_unix(double alt_deg, bool morning) const {
    double h = ha(alt_deg);
    if (std::isnan(h)) return std::numeric_limits<double>::quiet_NaN();
    return day_utc * 86400.0 + (morning ? noon_hour - h : noon_hour + h) * 3600.0;
  }

  double noon_unix() const { return day_utc * 86400.0 + noon_hour * 3600.0; }

  double midnight_unix() const {
    double mh = noon_hour - 12.0;
    if (mh < 0) mh += 24.0;
    return day_utc * 86400.0 + mh * 3600.0;
  }
};

// ============================================================================
// SolarGeometryCache: memoises SolarGeometry by UTC day index.
//
// Time series typically share a smaller set of unique UTC days, so caching
// avoids recomputing the full solar position for every array element.
// ============================================================================

struct GeomKey {
  double day_utc;
  bool operator==(const GeomKey& o) const { return day_utc == o.day_utc; }
};
struct GeomKeyHash {
  std::size_t operator()(const GeomKey& k) const {
    // day_utc is an integer-valued double; reinterpret bits for hashing.
    std::size_t bits;
    static_assert(sizeof(bits) == sizeof(k.day_utc), "size mismatch");
    std::memcpy(&bits, &k.day_utc, sizeof(bits));
    return bits;
  }
};

struct SolarGeometryCache {
  SolarGeometryCache(double lat, double lon) : lat_deg_(lat), lon_deg_(lon) {}

  const SolarGeometry& get(double day_utc) {
    GeomKey key{day_utc};
    auto it = cache_.find(key);
    if (it != cache_.end()) return it->second;
    auto [ins, ok] = cache_.emplace(key, SolarGeometry(day_utc, lat_deg_, lon_deg_));
    return ins->second;
  }

private:
  double lat_deg_, lon_deg_;
  std::unordered_map<GeomKey, SolarGeometry, GeomKeyHash> cache_;
};

// ============================================================================
// SolarDayBounds: midnight_d / noon_d / midnight_d1 for one solar day
//
// Solar noon is defined by the equation of time and need not fall within the
// UTC calendar day that shares its date.  For locations well west of the
// central meridian of their UTC offset, noon can spill into the *next* UTC
// calendar day.  We therefore pick noon from whichever UTC day actually
// contains it within [midnight_d, midnight_d1).
// ============================================================================

struct SolarDayBounds {
  double midnight_d;
  double noon_d;
  double midnight_d1;
  bool   valid;

  SolarDayBounds(double d, SolarGeometryCache& cache)
    : midnight_d(0), noon_d(0), midnight_d1(0), valid(false) {
    const SolarGeometry& sd = cache.get(d);
    if (!sd.valid) return;
    midnight_d = sd.midnight_unix();

    const SolarGeometry& next_sd = cache.get(d + 1.0);
    if (!next_sd.valid) return;
    midnight_d1 = next_sd.midnight_unix();

    // Solar noon may fall on today's UTC day or the next one.  Choose the
    // candidate that lies within [midnight_d, midnight_d1).
    double noon_today = sd.noon_unix();
    double noon_next  = next_sd.noon_unix();
    if (!std::isnan(noon_today) && noon_today >= midnight_d && noon_today < midnight_d1)
      noon_d = noon_today;
    else if (!std::isnan(noon_next) && noon_next >= midnight_d && noon_next < midnight_d1)
      noon_d = noon_next;
    else
      return; // noon not found in this solar day — leave valid = false

    valid = !std::isnan(midnight_d) && !std::isnan(midnight_d1);
  }
};

// ============================================================================
// Solar day conversion helpers (midnight-to-midnight)
//
// A "solar day" begins and ends at solar midnight (the Sun's anti-transit,
// minimum altitude).  The integer part of a solar day count is the number of
// solar midnights elapsed since J2000; the fractional part is the proportion
// of the current solar day that has passed.
// ============================================================================

// Returns the solar day index for a UTC timestamp
static double solar_day_index_from_utc(double ut, SolarGeometryCache& cache) {
  double day_utc = std::floor(ut / 86400.0);
  const SolarGeometry& today = cache.get(day_utc);
  if (!today.valid) return std::numeric_limits<double>::quiet_NaN();
  double mid_today = today.midnight_unix();
  return (ut < mid_today) ? (day_utc - 1.0) : day_utc;
}

// Convert a vector of UTC Unix timestamps to continuous solar day counts.
// The integer boundary of each solar day falls at solar midnight.
static doubles solar_days_from_utc_impl(doubles unix_times,
                                         double lat_deg, double lon_deg,
                                         double alt_deg = 0.0) {
  int n = unix_times.size();
  writable::doubles result(n);
  SolarGeometryCache cache(lat_deg, lon_deg);

  for (int i = 0; i < n; i++) {
    double ut = unix_times[i];
    if (std::isnan(ut)) { result[i] = NA_REAL; continue; }

    double day_utc = solar_day_index_from_utc(ut, cache);
    if (std::isnan(day_utc)) { result[i] = NA_REAL; continue; }

    SolarDayBounds bounds(day_utc, cache);
    if (!bounds.valid) { result[i] = NA_REAL; continue; }

    result[i] = day_utc + (ut - bounds.midnight_d)
                        / (bounds.midnight_d1 - bounds.midnight_d);
  }
  return result;
}

// Convert continuous solar day counts back to UTC Unix timestamps.
static doubles utc_from_solar_days_impl(doubles solar_day_counts,
                                         double lat_deg, double lon_deg,
                                         double alt_deg = 0.0) {
  int n = solar_day_counts.size();
  writable::doubles result(n);
  SolarGeometryCache cache(lat_deg, lon_deg);

  for (int i = 0; i < n; i++) {
    double count = solar_day_counts[i];
    if (std::isnan(count)) { result[i] = NA_REAL; continue; }

    double complete = std::floor(count);
    double fraction = count - complete;
    double day_utc  = complete;

    const SolarGeometry& today = cache.get(day_utc);
    const SolarGeometry& next  = cache.get(day_utc + 1.0);
    if (!today.valid || !next.valid) { result[i] = NA_REAL; continue; }

    double mid_unix      = today.midnight_unix();
    double next_mid_unix = next.midnight_unix();

    result[i] = mid_unix + fraction * (next_mid_unix - mid_unix);
  }
  return result;
}

// ============================================================================
// Solar phase boundary helper
//
// Returns the UTC Unix timestamp of illumination phase boundary b for the
// solar day whose geometry spans [midnight_d, midnight_d1).
//
// Boundaries b = 0..7 are the eight twilight/horizon events, in order:
//   0 = astro dawn    (alt = -18 deg, morning)
//   1 = nautical dawn (alt = -12 deg, morning)
//   2 = civil dawn    (alt =  -6 deg, morning)
//   3 = sunrise       (alt = -0.833 deg, morning)
//   4 = sunset        (alt = -0.833 deg, evening)
//   5 = civil dusk    (alt =  -6 deg, evening)
//   6 = nautical dusk (alt = -12 deg, evening)
//   7 = astro dusk    (alt = -18 deg, evening)
//
// Phase p spans boundary p to boundary (p+1) % 8, wrapping across midnight.
// Night (phase 7) spans astro dusk to the next day's astro dawn.
// Returns NaN for polar conditions where the event does not occur.
//
// alt_deg: observer elevation correction added to each altitude threshold.
// ============================================================================

static double solar_phase_boundary_from_geom(const SolarGeometry& today,
                                              const SolarGeometry& next,
                                              int b, double alt_deg) {
  static constexpr double thresholds[] = {
    -18.0,    // b=0 astro dawn
    -12.0,    // b=1 nautical dawn
    -6.0,     // b=2 civil dawn
    -0.833,   // b=3 sunrise
    -0.833,   // b=4 sunset
    -6.0,     // b=5 civil dusk
    -12.0,    // b=6 nautical dusk
    -18.0,    // b=7 astro dusk
  };

  bool   morning = (b <= 3);
  double alt     = thresholds[b] + alt_deg;

  // All solar events fall within 12 hours of solar noon, so use a
  // [noon - 12h, noon + 12h) window rather than a midnight window.
  // This is correct for any UTC offset without needing midnight_unix().
  double noon     = today.noon_unix();
  double window_lo = noon - 12.0 * 3600.0;
  double window_hi = noon + 12.0 * 3600.0;

  double t = today.event_unix(alt, morning);

  // Handle UTC-day wraparound (noon near UTC midnight)
  if (!std::isnan(t) && t < window_lo)
    t = next.event_unix(alt, morning);

  // Clamp out-of-range (polar day/night)
  if (!std::isnan(t) && (t < window_lo || t >= window_hi))
    t = std::numeric_limits<double>::quiet_NaN();

  return t;
}


// ============================================================================
// Exported R functions — solar
// ============================================================================

// UTC Unix timestamps -> continuous solar day count (midnight-based).
// Integer part = number of solar midnights elapsed since J2000.
// Fractional part = proportion of current solar day elapsed [0, 1).
[[cpp11::register]]
doubles approx_solar_days_from_utc(doubles unix_times,
                                    double lat_deg, double lon_deg,
                                    double alt_deg = 0.0) {
  return solar_days_from_utc_impl(unix_times, lat_deg, lon_deg, alt_deg);
}

// Continuous solar day counts -> UTC Unix timestamps.
[[cpp11::register]]
doubles approx_utc_from_solar_days(doubles solar_day_counts,
                                    double lat_deg, double lon_deg,
                                    double alt_deg = 0.0) {
  return utc_from_solar_days_impl(solar_day_counts, lat_deg, lon_deg, alt_deg);
}

// Continuous solar phase counts -> UTC Unix timestamps.
//
// Each phase count encodes solar_day_index * 8 + phase_index, and the
// fractional part interpolates linearly within that phase.
//
// Phase p spans boundary p to boundary (p+1) % 8.  Phase 7 (night) wraps
// across solar midnight: t0 = astro dusk of day d, t1 = astro dawn of day d+1.
[[cpp11::register]]
doubles approx_solar_phase_utc(doubles phase_counts,
                                double lat_deg, double lon_deg,
                                double alt_deg = 0.0) {
  int n = phase_counts.size();
  writable::doubles result(n);
  SolarGeometryCache cache(lat_deg, lon_deg);

  for (int i = 0; i < n; i++) {
    double pc = phase_counts[i];
    if (std::isnan(pc)) { result[i] = NA_REAL; continue; }

    double pf_floor  = std::floor(pc);
    double alpha     = pc - pf_floor; // fractional part [0, 1)

    double d         = std::floor(pf_floor / 8.0);
    int    phase_idx = (int)std::fmod(pf_floor, 8.0);
    if (phase_idx < 0) { phase_idx += 8; d -= 1.0; }

    const SolarGeometry& today = cache.get(d);
    if (!today.valid) { result[i] = NA_REAL; continue; }
    const SolarGeometry& next = cache.get(d + 1.0);
    if (!next.valid) { result[i] = NA_REAL; continue; }
    // t0: start of this phase (boundary phase_idx of day d)
    double t0 = solar_phase_boundary_from_geom(today, next, phase_idx, alt_deg);
    if (std::isnan(t0)) { result[i] = NA_REAL; continue; }

    if (alpha == 0.0) { result[i] = t0; continue; }

    // t1: end of this phase.  Phase 7 (night) wraps: t1 = astro dawn of d+1.
    double t1;
    if (phase_idx == 7) {
      const SolarGeometry& next2 = cache.get(d + 2.0);
      if (!next2.valid) { result[i] = NA_REAL; continue; }
      t1 = solar_phase_boundary_from_geom(next, next2, 0, alt_deg);
    } else {
      t1 = solar_phase_boundary_from_geom(today, next, phase_idx + 1, alt_deg);
    }
    if (std::isnan(t1)) { result[i] = NA_REAL; continue; }

    result[i] = t0 + alpha * (t1 - t0);
  }
  return result;
}


// ============================================================================
// Exported R functions — solar ampm (noon/midnight-based)
// ============================================================================

// UTC Unix timestamps -> continuous ampm count (solar_day * 2 + half + frac).
//
//   half = 0 (AM): solar midnight -> solar noon
//   half = 1 (PM): solar noon    -> solar midnight
//
// Noon is computed directly from the equation of time via SolarGeometry::noon_unix().
// Midnight is the solar anti-transit (SolarGeometry::midnight_unix()).
[[cpp11::register]]
doubles approx_solar_ampm_from_utc(doubles unix_times,
                                    double lat_deg, double lon_deg,
                                    double alt_deg = 0.0) {
  int n = unix_times.size();
  writable::doubles result(n);
  SolarGeometryCache cache(lat_deg, lon_deg);

  for (int i = 0; i < n; i++) {
    double ut = unix_times[i];
    if (std::isnan(ut)) { result[i] = NA_REAL; continue; }

    double d = solar_day_index_from_utc(ut, cache);
    if (std::isnan(d)) { result[i] = NA_REAL; continue; }

    SolarDayBounds bounds(d, cache);
    if (!bounds.valid) { result[i] = NA_REAL; continue; }
    double midnight_d  = bounds.midnight_d;
    double noon_d      = bounds.noon_d;
    double midnight_d1 = bounds.midnight_d1;

    double ampm_count;
    if (ut < noon_d) {
      ampm_count = d * 2.0 + (ut - midnight_d) / (noon_d - midnight_d);
    } else {
      ampm_count = d * 2.0 + 1.0 + (ut - noon_d) / (midnight_d1 - noon_d);
    }
    result[i] = ampm_count;
  }
  return result;
}

// Continuous ampm counts -> UTC Unix timestamps.
//
// Noon is reconstructed directly from SolarGeometry::noon_unix() (equation of time).
[[cpp11::register]]
doubles approx_utc_from_solar_ampm(doubles ampm_counts,
                                    double lat_deg, double lon_deg,
                                    double alt_deg = 0.0) {
  int n = ampm_counts.size();
  writable::doubles result(n);
  SolarGeometryCache cache(lat_deg, lon_deg);

  for (int i = 0; i < n; i++) {
    double ac = ampm_counts[i];
    if (std::isnan(ac)) { result[i] = NA_REAL; continue; }

    double pf   = std::floor(ac);
    double frac = ac - pf;
    int    half = (int)std::fmod(pf, 2.0);
    if (half < 0) half += 2;
    double d    = (pf - half) / 2.0;

    SolarDayBounds bounds(d, cache);
    if (!bounds.valid) { result[i] = NA_REAL; continue; }
    double midnight_d  = bounds.midnight_d;
    double noon_d      = bounds.noon_d;
    double midnight_d1 = bounds.midnight_d1;

    double ut;
    if (half == 0) {
      ut = midnight_d + frac * (noon_d - midnight_d);
    } else {
      ut = noon_d + frac * (midnight_d1 - noon_d);
    }
    result[i] = ut;
  }
  return result;
}

// UTC Unix timestamps -> continuous solar phase counts.
//
// The integer part encodes solar_day_index * 8 + phase_index (0..7), and the
// fractional part is the proportion of the current illumination phase elapsed.
// Phase 7 (night) spans astro dusk of day d to astro dawn of day d+1.
// Returns NA for polar conditions where a boundary is undefined.

// Cached boundaries for one solar day, used by approx_solar_phase_from_utc.
struct PhaseBoundaryCache {
  double sd          = 0;      // solar day index this cache is valid for
  double b[8]        = {};     // boundaries 0..7 for day sd
  double dawn_next   = 0;      // boundary 0 (astro dawn) of day sd+1
  bool   valid       = false;
};

// Recompute phase boundaries for solar day `sd` into `out`.
static bool recompute_phase_boundaries(double sd, double alt_deg,
                                        SolarGeometryCache& cache,
                                        PhaseBoundaryCache& out) {
  out.valid = false;
  out.sd    = sd;

  const SolarGeometry& today = cache.get(sd);
  const SolarGeometry& next  = cache.get(sd + 1.0);
  if (!today.valid || !next.valid) return false;

  for (int k = 0; k < 8; k++)
    out.b[k] = solar_phase_boundary_from_geom(today, next, k, alt_deg);

  // Astro dawn of the next solar day (end of night phase 7).
  const SolarGeometry& next2 = cache.get(sd + 2.0);
  if (!next2.valid) return false;
  out.dawn_next = solar_phase_boundary_from_geom(next, next2, 0, alt_deg);

  // Require at minimum that astro dawn (b=0) and astro dusk (b=7) are defined.
  if (std::isnan(out.b[0]) || std::isnan(out.b[7])) return false;

  out.valid = true;
  return true;
}

[[cpp11::register]]
doubles approx_solar_phase_from_utc(doubles unix_times,
                                     double lat_deg, double lon_deg,
                                     double alt_deg = 0.0) {
  int n = unix_times.size();
  writable::doubles result(n);
  SolarGeometryCache cache(lat_deg, lon_deg);

  double last_sd = std::numeric_limits<double>::quiet_NaN();
  PhaseBoundaryCache pcache;

  for (int i = 0; i < n; i++) {
    double ut = unix_times[i];
    if (std::isnan(ut)) { result[i] = NA_REAL; continue; }

    // Solar event windows are centred on solar noon (noon ± 12 h).  For
    // east-longitude locations, noon falls in a later UTC calendar day than
    // the events in the pre-dawn portion of the same solar day.  Try
    // sd+1, sd, and sd-1 to cover all cases.
    double sd = std::floor(ut / 86400.0) + 1.0;

    result[i] = NA_REAL;

    for (int attempt = 0; attempt <= 2 && std::isnan(result[i]); attempt++, sd -= 1.0) {
      if (sd != last_sd) {
        last_sd = sd;
        recompute_phase_boundaries(sd, alt_deg, cache, pcache);
      }
      if (!pcache.valid) continue;

      // Phases 0..6: each spans [b[p], b[p+1]).
      for (int p = 0; p <= 6; p++) {
        double lo = pcache.b[p];
        double hi = pcache.b[p + 1];
        if (std::isnan(lo) || std::isnan(hi)) continue;
        if (ut >= lo && ut < hi) {
          result[i] = sd * 8.0 + p + (ut - lo) / (hi - lo);
          break;
        }
      }
      if (!std::isnan(result[i])) continue;

      // Phase 7 (night): spans [b[7], dawn_next) straddling solar midnight.
      double lo7 = pcache.b[7];
      double hi7 = pcache.dawn_next;
      if (!std::isnan(lo7) && !std::isnan(hi7) && ut >= lo7 && ut < hi7)
        result[i] = sd * 8.0 + 7.0 + (ut - lo7) / (hi7 - lo7);
    }
  }
  return result;
}

