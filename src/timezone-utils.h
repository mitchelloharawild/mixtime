#ifndef MIXTIME_TIMEZONE_UTILS_H
#define MIXTIME_TIMEZONE_UTILS_H

#include <tzdb/tzdb.h>
#include <tzdb/date.h>
#include <tzdb/tz.h>

#include <cpp11.hpp>

namespace mixtime {

// Load a timezone by name, with error handling
inline const date::time_zone* zone_name_load(const std::string& zone_name) {
  const date::time_zone* p_time_zone = nullptr;
  
  if (!tzdb::locate_zone(zone_name, p_time_zone)) {
    std::string message = "Failed to locate time zone: '" + zone_name + "'.";
    cpp11::stop(message.c_str());
  }
  
  return p_time_zone;
}

// Get sys_info for a given time point in a timezone
template <class Duration>
inline date::sys_info get_sys_info(
    const date::sys_time<Duration>& tp, 
    const date::time_zone* p_time_zone) {
  
  const date::sys_seconds ss = date::floor<std::chrono::seconds>(tp);
  date::sys_info info;
  
  if (!tzdb::get_sys_info(ss, p_time_zone, info)) {
    cpp11::stop("Failed to lookup information for the provided time zone.");
  }
  
  return info;
}

// Convert POSIXct (seconds since epoch) to sys_time
inline date::sys_seconds posixct_to_sys_time(double posixct) {
  // POSIXct is seconds since 1970-01-01 00:00:00 UTC
  auto tp = std::chrono::seconds(static_cast<int64_t>(posixct));
  return date::sys_seconds(tp);
}

// Convert sys_time back to POSIXct
inline double sys_time_to_posixct(const date::sys_seconds& tp) {
  return static_cast<double>(tp.time_since_epoch().count());
}

} // namespace mixtime

#endif
