# Parse time sequence strings into time units
# TODO - Generalise to be calendar-aware 
#        e.g. "year" being tu_year() or tu_isoyear()
parse_time_unit <- function(x) {
  by2 <- strsplit(x, " ", fixed = TRUE)[[1L]]
  if (length(by2) > 2L || length(by2) < 1L) 
      stop("invalid 'by' string")
  n <- if(length(by2) == 2L) as.integer(by2[[1L]]) else 1L
  x <- by2[[length(by2)]]
  
  switch(sub("s$", "", x),
    "second" = ,
    "sec" = tu_second(n),
    "minute" = ,
    "min" = tu_minute(n),
    "hour" = tu_hour(n),
    "day" = tu_day(n),
    "dstday" = tu_day(n), # DSTdays map to regular days
    "week" = tu_week(n),
    "month" = tu_month(n),
    "year" = tu_year(n),
    "quarter" = tu_quarter(n),
    stop("Unknown time unit: '", x, "'. Valid units are: secs, mins, hours, days, weeks, months, years, DSTdays, quarters")
  )
}
