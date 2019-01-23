## Test environments
* local OS X install, R 3.5.2
* ubuntu 14.04 (on travis-ci), R 3.5.2
* win-builder (devel and release)

## R CMD check results

0 errors | 2 warnings | 1 note

* 1 note: This is a new release.
* 2 warnings, both are:
  Registered S3 methods overwritten by 'bain':
      t.test.default stats
      t.test.formula stats
  This behavior is intentional. The t.test methods in 'stats' do not return
  variance or sample size, which are both required for Bayesian inference.
  Therefore, I am intentionally masking the 'stats' t.test methods with a
  version that returns variance and sample size. No other changes to the code
  of the stats::t.test functions. This behavior is explained in the manual, and
  the bain::t.test methods return an object with a different class than the
  stats::t.tests. If users try to run bain on a stats::t.test object, they
  receive an error.
