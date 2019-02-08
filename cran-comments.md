# Version 0.2.1

As pointed out by Prof. Brian Ripley,
https://cran.r-project.org/web/checks/check_results_bain.html fails for
r-patched-solaris-x86. This version addresses the reason for the compilation 
error of the F90 source code:

* Every END statement in the F90 code was ammended to explicitly state which
  subroutine is being ended.

# Resubmission 2/8/2019

Requesting a ‘same-version update’: As pointed out by Prof. Brian Ripley,
https://cran.r-project.org/web/checks/check_results_bain.html fails for
r-patched-solaris-x86. This is a resubmission addressing Prof. Brian Ripley's
comment:

* Every END statement in the F90 code was ammended to explicitly state which
  subroutine is being ended.

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
