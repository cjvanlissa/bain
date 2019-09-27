
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![CRAN
status](https://www.r-pkg.org/badges/version/bain)](https://cran.r-project.org/package=bain)
[![Build
Status](https://travis-ci.org/cjvanlissa/bain.svg?branch=master)](https://travis-ci.org/cjvanlissa/bain)
[![](https://cranlogs.r-pkg.org/badges/tidyLPA)](https://cran.r-project.org/package=tidyLPA)

# bain

Bain stands for Bayesian informative hypothesis evaluation. It computes
Bayes factors for informative hypotheses in a wide variety of
statistical models. Just run your analysis as usual, and then apply bain
to the output. A tutorial is available at
[DOI:10.31234/osf.io/v3shc](https://psyarxiv.com/v3shc/).

## Installation

Install `bain` from CRAN:

``` r
install.packages("bain")
```

## Workflow

Add bain to your existing R workflow, and obtain Bayes factors for your
familiar R analyses\! Bain is compatible with the pipe operator. Here is
an example for testing an informative hypothesis about mean differences
in an ANOVA:

``` r
# Load bain
library(bain)
# dplyr to access the %>% operator
library(dplyr)
# Iris as example data
iris %>%
  # Select outcome and predictor variables
  select(Sepal.Length, Species) %>%      
  # Add -1 to the formula to estimate group means, as in ANOVA
  lm(Sepal.Length ~ -1 + Species, .) %>% 
  bain("Speciessetosa < Speciesversicolor = Speciesvirginica;
       Speciessetosa < Speciesversicolor < Speciesvirginica")
#> Bayesian informative hypothesis testing for an object of class lm (ANOVA):
#> 
#>    Fit_eq Com_eq Fit_in Com_in Fit   Com   BF              PMPa  PMPb 
#> H1 0.000  0.447  1.000  0.500  0.000 0.224 0.000           0.000 0.000
#> H2 1.000  1.000  1.000  0.168  1.000 0.168 83012471303.046 1.000 0.856
#> Hu                                                               0.144
#> 
#> Hypotheses:
#>   H1: Speciessetosa<Speciesversicolor=Speciesvirginica
#>   H2: Speciessetosa<Speciesversicolor<Speciesvirginica
#> 
#> Note: BF denotes the Bayes factor of the hypothesis at hand versus its complement.
```
