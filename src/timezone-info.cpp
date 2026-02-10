#include "timezone-utils.h"

#include <tzdb/tzdb.h>
#include <tzdb/date.h>
#include <tzdb/tz.h>

#include <cpp11.hpp>
#include <R.h>
#include <Rinternals.h>

using namespace cpp11;

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
  
  for (R_xlen_t i = 0; i < size; ++i) {
    if (ISNA(posixct[i])) {
      offset[i] = NA_REAL;
      continue;
    }
    
    const date::time_zone* p_time_zone_elt;
    if (recycle_zone) {
      p_time_zone_elt = p_time_zone;
    } else {
      const std::string zone_name_elt = r_string(zone[i]);
      p_time_zone_elt = mixtime::zone_name_load(zone_name_elt);
    }
    
    const date::sys_seconds tp = mixtime::posixct_to_sys_time(posixct[i]);
    const date::sys_info info = mixtime::get_sys_info(tp, p_time_zone_elt);
    
    offset[i] = static_cast<double>(info.offset.count());
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
  
  for (R_xlen_t i = 0; i < size; ++i) {
    if (ISNA(posixct[i])) {
      SET_STRING_ELT(abbreviation, i, NA_STRING);
      continue;
    }
    
    const date::time_zone* p_time_zone_elt;
    if (recycle_zone) {
      p_time_zone_elt = p_time_zone;
    } else {
      const std::string zone_name_elt = r_string(zone[i]);
      p_time_zone_elt = mixtime::zone_name_load(zone_name_elt);
    }
    
    const date::sys_seconds tp = mixtime::posixct_to_sys_time(posixct[i]);
    const date::sys_info info = mixtime::get_sys_info(tp, p_time_zone_elt);
    
    SET_STRING_ELT(
      abbreviation, i,
      Rf_mkCharLenCE(info.abbrev.c_str(), info.abbrev.size(), CE_UTF8)
    );
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