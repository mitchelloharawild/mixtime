## Test environments
* local ubuntu 24.04 install, R 4.6.2
* ubuntu-latest (on GitHub actions), R-release
* win-builder, R-devel

## R CMD check results

0 errors | 0 warnings | 0 notes

## Revdep checks

The only reverse dependency, ggtime, was checked. None have changed to worse.

ggtime 1.1.1 already fails its tests on CRAN's check machines. This release
fixes its examples ERROR, and reduces its test failures from 153 to 7. The
remaining 7 failures are caused by ggtime's use of `tidyr::gather()`, which
does not keep vecvec (>= 1.5.0) vectors intact. They fail in the same way with
the current CRAN version of mixtime. A fixed ggtime release will be submitted
right after this one.
