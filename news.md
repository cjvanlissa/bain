# bain 0.2.8

* Minor bugfix to ensure compatibility with JASP

# bain 0.2.7

* Add column PMPc to bain output object and printed output table
* Add row Hc to bain output object and printed output table
* Fix large integer bug in Fortran code

# bain 0.2.6

* Fix URL to code of conduct as per CRAN request

# bain 0.2.5

* Fix bug in bain:::bain.t_test() on Debian linux
* Fix bug in parser related to decimal numbers without leading zero (issue #28)
  
# bain 0.2.4

* Add bain_sensitivity() and S3 method summary.bain_sensitivity()
* Change default stats for print.bain() to omit columns that are not relevant  
  for applied researchers

# bain 0.2.3

* Fix lavaan bug: Within-group constraints were flagged as between-group constraints
* Fix bain.lm bug: Parameters were not selected, leading to error if not all model parameters were mentioned in the hypothesis
* Fix bain.lm bug for standardize = TRUE: Sigma was not named correctly
* Add internal functions to use the CRAN-version of bain in JASP
* Simplified functions to get lavaan estimates, and made them more interoperable for dependent R-packages BFpack and gorica.
* Add testthat test files
* Update readme.md
* Update Vignette

# bain 0.2.2

* Added S3 function for lavaan structural equation models
* Updated sesamesim data set
* Improved documentation
* Added informative error messages
* Added checks for legal input
* Added Camiel van Zundert as contributor
  
# bain 0.2.1

* Fixed compilation error of F90 source code on r-patched-solaris-x86, by 
  explicitly ending subroutines


# bain 0.2.0

* First CRAN release. Version 0.1.0 is available at https://informative-hypotheses.sites.uu.nl/software/bain/
