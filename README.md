
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Build
Status](https://travis-ci.org/cjvanlissa/bain.svg?branch=master)](https://travis-ci.org/cjvanlissa/bain)
<!--[![CRAN status](https://www.r-pkg.org/badges/version/tidyLPA)](https://cran.r-project.org/package=tidyLPA)-->
<!--[![](https://cranlogs.r-pkg.org/badges/tidyLPA)](https://cran.r-project.org/package=tidyLPA)-->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- [![DOI](http://joss.theoj.org/papers/10.21105/joss.00978/status.svg)](10.1111/bmsp.12110)-->

# bain

Bain is an abbreviation for Bayesian informative hypothesis evaluation.
It uses the Bayes factor to evaluate equality and inequality constraint
hypotheses in a wide variety of statistical models.

## Installation

You can install bain from github with:

``` r
# install.packages("devtools")
devtools::install_github("cjvanlissa/bain", args = c("--no-multiarch", "--no-test-load"))
```

## Workflow

Add bain to your existing R workflow, and obtain Bayes factors for your
familiar R analyses\! Bain is compatible with the pipe operator. Here is
an example for testing an informative hypothesis about mean differences
in an ANOVA:

``` r
# Load dplyr to access the pipe operator
library(dplyr)
iris %>%                                 # Example data
  select(Sepal.Length, Species) %>%      # Select outcome and predictor variables
  lm(Sepal.Length ~ -1 + Species, .) %>% # Add -1 to the formula to estimate group means, as in ANOVA
  bain("setosa < versicolor = virginica; setosa < versicolor < virginica")
```
