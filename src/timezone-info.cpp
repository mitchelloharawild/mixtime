#include "timezone-utils.h"

#include <tzdb/tzdb.h>
#include <tzdb/date.h>
#include <tzdb/tz.h>

#include <cpp11.hpp>
#include <R.h>
#include <Rinternals.h>

#include <algorithm>
#include <unordered_map>
#include <vector>

using namespace cpp11;

namespace {

// Offset periods are cached per zone for instants in these years. Each lookup
// in tzdb costs a few microseconds (much more after its transition table
// ends, where daylight saving rules are evaluated), while a binary search of
// the cache costs nanoseconds. Outside these years tzdb is asked directly.
const date::sys_seconds cache_from{date::sys_days{date::year{1000} / 1 / 1}};
const date::sys_seconds cache_to{date::sys_days{date::year{4000} / 1 / 1}};

// At most this many periods are cached for each zone (about 640 KB)
constexpr std::size_t max_cached_periods = 10000;

// Contiguous offset periods of a zone, in order, exactly as tzdb gives them,
// and the position of the last one looked up
struct zone_periods {
  std::vector<date::sys_info> periods;
  std::size_t last = 0;
};

std::unordered_map<const date::time_zone*, zone_periods>& period_cache() {
  static std::unordered_map<const date::time_zone*, zone_periods> cache;
  return cache;
}

// Holds a period that tzdb gives outside the cache
date::sys_info& uncached_sys_info() {
  static date::sys_info info;
  return info;
}

const date::sys_info& lookup_uncached(
    const date::sys_seconds tp,
    const date::time_zone* p_time_zone) {
  date::sys_info& info = uncached_sys_info();
  info = mixtime::get_sys_info(tp, p_time_zone);
  return info;
}

// The offset period containing `tp`, as tzdb would give it. The cache only
// grows from what tzdb returns, period by period towards `tp`, so it holds
// just the span of the instants looked up. The result is valid until the next
// lookup.
const date::sys_info& lookup_sys_info(
    const date::sys_seconds tp,
    const date::time_zone* p_time_zone) {

  if (tp < cache_from || tp >= cache_to) {
    return lookup_uncached(tp, p_time_zone);
  }

  zone_periods& zone = period_cache()[p_time_zone];
  std::vector<date::sys_info>& periods = zone.periods;

  if (periods.empty()) {
    periods.push_back(mixtime::get_sys_info(tp, p_time_zone));
    zone.last = 0;
    return periods.back();
  }

  if (tp >= periods.back().end) {
    // Walk forward from the end of the cache. Every period start asked for
    // is at most `tp`, so within the cached years.
    while (periods.size() < max_cached_periods) {
      const date::sys_seconds end = periods.back().end;
      date::sys_info next = mixtime::get_sys_info(end, p_time_zone);
      if (next.begin != end || next.end <= end) {
        break;
      }
      periods.push_back(std::move(next));
      if (tp < periods.back().end) {
        zone.last = periods.size() - 1;
        return periods.back();
      }
    }
    return lookup_uncached(tp, p_time_zone);
  }

  if (tp < periods.front().begin) {
    // Walk backward from the start of the cache, then prepend the periods
    // found in one go
    std::vector<date::sys_info> earlier;
    date::sys_seconds begin = periods.front().begin;
    bool found = false;
    while (periods.size() + earlier.size() < max_cached_periods) {
      date::sys_info prev = mixtime::get_sys_info(
        begin - std::chrono::seconds{1}, p_time_zone
      );
      if (prev.end != begin || prev.begin >= begin) {
        break;
      }
      begin = prev.begin;
      earlier.push_back(std::move(prev));
      if (tp >= begin) {
        found = true;
        break;
      }
    }
    periods.insert(periods.begin(), earlier.rbegin(), earlier.rend());
    zone.last = 0;
    return found ? periods.front() : lookup_uncached(tp, p_time_zone);
  }

  // Sorted times usually move on to the next period
  const std::size_t next = zone.last + 1;
  if (next < periods.size() && periods[next].begin <= tp && tp < periods[next].end) {
    zone.last = next;
    return periods[next];
  }

  // Otherwise, the last period starting at or before `tp`
  const auto it = std::upper_bound(
    periods.begin(), periods.end(), tp,
    [](const date::sys_seconds& t, const date::sys_info& p) {
      return t < p.begin;
    }
  );
  zone.last = static_cast<std::size_t>(it - periods.begin()) - 1;
  return periods[zone.last];
}

} // namespace

// Get just the UTC offset for a vector of times (simplified version)
[[cpp11::register]]
writable::doubles get_tz_offset(
    const doubles& posixct,
    const strings& zone) {
  
  const R_xlen_t size = posixct.size();
  const bool recycle_zone = zone.size() == 1;
  
  writable::doubles offset(size);
  
  // Pre-load timezone if recycling
  const date::time_zone* p_time_zone = nullptr;
  if (recycle_zone) {
    const std::string zone_name = r_string(zone[0]);
    p_time_zone = mixtime::zone_name_load(zone_name);
  }
  
  // The zone and offset period of the previous lookup, reused while times
  // stay in them
  SEXP zone_elt = R_NilValue;
  date::sys_seconds begin;
  date::sys_seconds end;
  double period_offset = 0;
  bool cached = false;

  for (R_xlen_t i = 0; i < size; ++i) {
    if (ISNA(posixct[i])) {
      offset[i] = NA_REAL;
      continue;
    }
    
    // R stores each distinct string once, so a repeated zone is the same
    // pointer as the previous element's
    if (!recycle_zone && STRING_ELT(zone, i) != zone_elt) {
      zone_elt = STRING_ELT(zone, i);
      p_time_zone = mixtime::zone_name_load(r_string(zone[i]));
      cached = false;
    }
    
    const date::sys_seconds tp = mixtime::posixct_to_sys_time(posixct[i]);
    if (!cached || tp < begin || tp >= end) {
      const date::sys_info& info = lookup_sys_info(tp, p_time_zone);
      begin = info.begin;
      end = info.end;
      period_offset = static_cast<double>(info.offset.count());
      cached = true;
    }
    
    offset[i] = period_offset;
  }
  
  return offset;
}

// Get timezone abbreviation for a vector of times
[[cpp11::register]]
writable::strings get_tz_abbreviation(
    const doubles& posixct,
    const strings& zone) {
  
  const R_xlen_t size = posixct.size();
  const bool recycle_zone = zone.size() == 1;
  
  writable::strings abbreviation(size);
  
  // Pre-load timezone if recycling
  const date::time_zone* p_time_zone = nullptr;
  if (recycle_zone) {
    const std::string zone_name = r_string(zone[0]);
    p_time_zone = mixtime::zone_name_load(zone_name);
  }
  
  // The zone, offset period and abbreviation of the previous lookup, reused
  // while times stay in them
  SEXP zone_elt = R_NilValue;
  date::sys_info info;
  SEXP abbrev = NA_STRING;
  bool cached = false;

  for (R_xlen_t i = 0; i < size; ++i) {
    if (ISNA(posixct[i])) {
      SET_STRING_ELT(abbreviation, i, NA_STRING);
      continue;
    }
    
    if (!recycle_zone && STRING_ELT(zone, i) != zone_elt) {
      zone_elt = STRING_ELT(zone, i);
      p_time_zone = mixtime::zone_name_load(r_string(zone[i]));
      cached = false;
    }
    
    const date::sys_seconds tp = mixtime::posixct_to_sys_time(posixct[i]);
    if (!cached || tp < info.begin || tp >= info.end) {
      info = lookup_sys_info(tp, p_time_zone);
      // Kept reachable by `abbreviation`, which it is stored in straight away
      abbrev = Rf_mkCharLenCE(info.abbrev.c_str(), info.abbrev.size(), CE_UTF8);
      cached = true;
    }
    
    SET_STRING_ELT(abbreviation, i, abbrev);
  }
  
  return abbreviation;
}


// Get all timezone transitions between two time points
[[cpp11::register]]
writable::list get_tz_transitions(
    const double start_posixct,
    const double end_posixct,
    const std::string& zone_name) {
  
  // Handle NA values
  if (ISNA(start_posixct) || ISNA(end_posixct)) {
    cpp11::stop("start and end cannot be NA");
  }
  
  // Check that start < end
  if (start_posixct >= end_posixct) {
    cpp11::stop("start must be before end");
  }
  
  // Load timezone
  const date::time_zone* p_time_zone = mixtime::zone_name_load(zone_name);
  
  // Convert to sys_time
  const date::sys_seconds start_tp = mixtime::posixct_to_sys_time(start_posixct);
  const date::sys_seconds end_tp = mixtime::posixct_to_sys_time(end_posixct);
  
  // Collect transitions
  std::vector<double> transition_times;
  std::vector<double> offsets_before;
  std::vector<double> offsets_after;
  // std::vector<bool> dst_before;
  // std::vector<bool> dst_after;
  // std::vector<std::string> abbrev_before;
  // std::vector<std::string> abbrev_after;
  
  const std::chrono::minutes zero{0};
  
  // Get info at start to find first transition
  date::sys_info current_info = mixtime::get_sys_info(start_tp, p_time_zone);
  date::sys_seconds next_transition = date::sys_seconds(current_info.end.time_since_epoch());
  
  // Collect all transitions between start and end
  while (next_transition < end_tp) {
    // Get info before and after the transition
    date::sys_info info_before = mixtime::get_sys_info(next_transition - std::chrono::seconds{1}, p_time_zone);
    date::sys_info info_after = mixtime::get_sys_info(next_transition, p_time_zone);
    
    // Store transition information
    transition_times.push_back(mixtime::sys_time_to_posixct(next_transition));
    offsets_before.push_back(static_cast<double>(info_before.offset.count()));
    offsets_after.push_back(static_cast<double>(info_after.offset.count()));
    // dst_before.push_back(info_before.save != zero);
    // dst_after.push_back(info_after.save != zero);
    // abbrev_before.push_back(info_before.abbrev);
    // abbrev_after.push_back(info_after.abbrev);
    
    // Move to next transition using info_after
    next_transition = date::sys_seconds(info_after.end.time_since_epoch());
  }
  
  // Convert to R vectors
  const R_xlen_t n = transition_times.size();
  
  writable::doubles times(n);
  writable::doubles offset_before(n);
  writable::doubles offset_after(n);
  // writable::logicals dst_before_logical(n);
  // writable::logicals dst_after_logical(n);
  // writable::strings abbrev_before_str(n);
  // writable::strings abbrev_after_str(n);
  
  for (R_xlen_t i = 0; i < n; ++i) {
    times[i] = transition_times[i];
    offset_before[i] = offsets_before[i];
    offset_after[i] = offsets_after[i];
    // dst_before_logical[i] = static_cast<bool>(dst_before[i]);
    // dst_after_logical[i] = static_cast<bool>(dst_after[i]);
    // SET_STRING_ELT(
    //   abbrev_before_str, i,
    //   Rf_mkCharLenCE(abbrev_before[i].c_str(), abbrev_before[i].size(), CE_UTF8)
    // );
    // SET_STRING_ELT(
    //   abbrev_after_str, i,
    //   Rf_mkCharLenCE(abbrev_after[i].c_str(), abbrev_after[i].size(), CE_UTF8)
    // );
  }
  
  writable::data_frame out({
    "time"_nm = times,
    "offset_before"_nm = offset_before,
    "offset_after"_nm = offset_after
    // "dst_before"_nm = dst_before_logical,
    // "dst_after"_nm = dst_after_logical,
    // "abbreviation_before"_nm = abbrev_before_str,
    // "abbreviation_after"_nm = abbrev_after_str
  });
  return out;
}