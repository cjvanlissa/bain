# Version 0.2.3

This resubmission attempts to address the error in the incoming checks for Debian.
The package only fails for Debian. I cannot reproduce the error on any of the 11 testing environments listed below (including rhub::check_for_cran()). 

The error is: --- failure: the condition has length > 1 ---

Triggered by this line: if (!class(x$coefficients) == "numeric")
 
I have replaced this with a more robust check:

if(!("numeric" %in% class(x$coefficients)) | !is.null(dim(x$coefficients)))

All tests below still pass. I hope this also addresses the mysterious Debian error.

## Test environments
* local Windows 10 install, R 3.6.1
* rhub check: Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* rhub check: Ubuntu Linux 16.04 LTS, R-release, GCC
* rhub check: Fedora Linux, R-devel, clang, gfortran
* rhub check: Debian Linux, R-devel, GCC ASAN/UBSAN
* win-builder (devel and release)
* travis-ci, OS X 10.13.3, R 3.6.1
* travis-ci, Ubuntu 16.04.6, R 3.5.3
* travis-ci, Ubuntu 16.04.6, R 3.6.1
* travis-ci, Ubuntu 16.04.6, R development version

## R CMD check results

0 errors | 0 warnings | 0 notes
