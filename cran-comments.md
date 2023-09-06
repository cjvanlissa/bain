# bain 0.2.9

* Update maintainer email address
* Change random number generation in Fortran function to comply with new CRAN
  policies. Seeds are consistent, but different from those in previous versions
  of bain. Thus, results produced with bain > 0.2.9 are computationally
  reproducible, but you cannot use bain > 0.2.9 to reproduce results from bain
  0.2.8 or lower.
* Add pbf() function to compute product Bayes factors


## Test environments

* local x86_64-pc-linux-gnu, R 4.2.2
* rhub check: Windows Server 2022, R-devel, 64 bit
* rhub check: Fedora Linux, R-devel, clang, gfortran
* rhub check: Ubuntu Linux 20.04.1 LTS, R-release, GCC
* rhub check: Debian Linux, R-devel, GCC ASAN/UBSAN
* win-builder: 4.2.3 (2023-03-15 ucrt)
* win-builder: 4.3.0 (2023-04-21 ucrt)
* win-builder: R Under development (unstable) (2023-04-25 r84327 ucrt)
* GitHub Actions: macos-latest, r: release 
* GitHub Actions: windows-latest, r: release 
* GitHub Actions: ubuntu-latest, r: devel, http-user-agent: release 
* GitHub Actions: ubuntu-latest, r: release 
* GitHub Actions: ubuntu-latest, r: oldrel-1

## R CMD check results

0 errors | 0 warnings | 1 note

* New maintainer
  + My affiliation changed, https://www.tilburguniversity.edu/nl/medewerkers/c-j-vanlissa
