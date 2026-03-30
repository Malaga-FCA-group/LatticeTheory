
# LatticeTheory

<!-- badges: start -->
  [![Codecov test coverage](https://codecov.io/gh/Malaga-FCA-group/LatticeTheory/branch/master/graph/badge.svg)](https://app.codecov.io/gh/Malaga-FCA-group/LatticeTheory?branch=master)
[![R-CMD-check](https://github.com/Malaga-FCA-group/LatticeTheory/workflows/R-CMD-check/badge.svg)](https://github.com/Malaga-FCA-group/LatticeTheory/actions)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of LatticeTheory is to implement functions related to LatticeTheory for the course on Algebraic Structures

## Installation

You can install the development version of LatticeTheory from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Malaga-FCA-group/LatticeTheory")
```
Tip: Make sure you have RTools installed according to your version of R
## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(LatticeTheory)
div <- divisors_lattice(210)
div$plot()
```