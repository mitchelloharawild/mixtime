# Some behaviour relies on development S7 (> 0.2.2): instances recording their
# original class rather than the calendar-attributed constructor copy, and
# generated constructors respecting overridden defaults of inherited properties.
skip_if_old_S7 <- function() {
  skip_if(packageVersion("S7") < "0.2.2.9000", "Requires development S7")
}
