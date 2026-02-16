# Parse time sequence strings into time units
# TODO - Generalise to be calendar-aware 
#        e.g. "year" being cal_gregorian$year() or cal_gregorian$isoyear()
parse_time_unit <- function(x) {
  by2 <- strsplit(x, " ", fixed = TRUE)[[1L]]
  if (length(by2) > 2L || length(by2) < 1L) 
      stop("invalid 'by' string")
  n <- if(length(by2) == 2L) as.integer(by2[[1L]]) else 1L
  x <- by2[[length(by2)]]
  
  switch(sub("s$", "", x),
    "second" = ,
    "sec" = cal_gregorian$second(n),
    "minute" = ,
    "min" = cal_gregorian$minute(n),
    "hour" = cal_gregorian$hour(n),
    "day" = cal_gregorian$day(n),
    "dstday" = cal_gregorian$day(n), # DSTdays map to regular days
    "week" = cal_isoweek$week(n),
    "month" = cal_gregorian$month(n),
    "year" = cal_gregorian$year(n),
    "quarter" = cal_gregorian$quarter(n),
    stop("Unknown time unit: '", x, "'. Valid units are: secs, mins, hours, days, weeks, months, years, DSTdays, quarters")
  )
}
