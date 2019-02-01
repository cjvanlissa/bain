# Resubmission

This is a resubmission addressing Swetlana Herbrandt's comments:

* Added references to Description field of DESCRIPTION file
* Removed \dontrun{} tags from examples. Please note that functions are also
  extensively tested on TravisCI, using the tests in tests/
* Examples for unexported functions are removed. These were part of a testing
  harness.
* Added authors as requested: John Burkardt (ctb), Niels Waller (ctb), and
  The R Core Team (cph).

# Initial submission

## Test environments
* local OS X install, R 3.5.2
* ubuntu 14.04 (on travis-ci), R 3.5.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* 1 note: This is a new release.
