PVPIAACL: Plausible Values estimation with the PIAAC-L data
================

R package for Bayesian estimation of latent trait distributions considering partially missing covariate data. Currently `PVPIAACL` allows the user to fit multidimensional latent regression item response models (LRMs). LRMs apply a multivariate regression equation to model the relationship between the latent trait and additional person covariates. Thus, they combine the fields of measurement models and structural analysis. LRMs are typically employed to generate plausible values in large-scale assessments.

Features
--------

-   Sampling from the posterior distribution of parameters is enriched by sampling from the full conditional distributions of missing values in person covariates.
-   Approximations for the distributions of missing values are constructed from classification and regression trees (CART).

Installing PVPIAACL
---------------

To install the latest development version from GitHub using the `devtools` package, run:

``` r
if(!require("devtools"))
  install.packages("devtools")
devtools::install_github("jcgaasch/PVPIAACL")
```

Dependencies
------------

PVPIAACL relies on some routines from other R packages, where the latest CRAN version is in use: `readstata13`, `MASS`, `rpart` and `Hmisc`.
