# bain 0.2.12

* Improve handling of parameter names in hypotheses
* Require explicit use of multiplication operator `*`
  when using pre-multipliers in hypotheses. E.g.,
  "2a > b" is now disallowed, and should be replaced with
  "2*a > b".

## Test environments

* local x86_64-pc-Windows 10 x64, R 4.6.0
* rhub check [VM] linux          R-* (any version)              ubuntu-la…
* rhub check [VM] m1-san         R-* (any version)              macos-15 …
* rhub check [VM] macos          R-* (any version)              macos-15-…
* rhub check [VM] macos-arm64    R-* (any version)              macos-lat…
* rhub check [VM] windows        R-* (any version)              windows-l…
* win-builder: release
* win-builder: oldrelease
* win-builder: development
* GitHub Actions: os: macos-latest,   r: 'release'
* GitHub Actions: os: windows-latest, r: 'release'
* GitHub Actions: os: ubuntu-latest,   r: 'devel', http-user-agent: 'release'
* GitHub Actions: os: ubuntu-latest,   r: 'release'
* GitHub Actions: os: ubuntu-latest,   r: 'oldrel-1'

## R CMD check results

0 errors | 0 warnings | 0 notes
