PVPIAACL: Plausible Values estimation with the PIAAC-L data
================

The Programme for the International Assessment of Adult Competencies-Longitudinal (PIAAC-L) Germany consortium partner Leibniz Institute for Educational Trajectories developed an R package which implements a Bayesian estimation algorithm that simultaneously generates plausible values (pvs) and imputes missing values in background variables. In addition to the pvs released in the PIAAC and PIAAC-L scientific use files, users can estimate pvs themselves specific to their research question, i.e., users select context variables from the PIAAC-L Scientific Use Files which are suitable for their analysis and directly enter the population model during pvs estimation. This estimation strategy addresses both item nonresponse in background variables as well as the ``curse of dimensionality'' due to the extensively large background information resulting from three waves of data collection in PIAAC-L. 

Currently `PVPIAACL` allows the user to fit multidimensional latent regression models. It applies a $D$-dimensional two-parameter normal ogive item response theory model

\[
P(y_{ijd}=1|\theta_{id},\alpha_{jd},\beta_{jd}) = \Phi(\alpha_{jd}\theta_{id} - \beta_{jd}),
\]

where $\theta_{id}$ denotes the individual latent ability, $\alpha_{jd}$ the item discriminations and $\beta_{jd}$ the item difficulty for dimension $d\in\{1,\ldots,D\}$ (Beguin & Glas, 2001) and a multivariate regression equation 

\[
\theta_i=x_i\Gamma + e_i,\quad e_i\overset{iid}{\sim}\mathcal{N}_D(0,\Sigma_e)
\]

to model the relationship between the latent trait and additional person covariates, where $x_i$ is a row vector of $K$ person covariates plus an intercept, $\Gamma$ a $(K+1)\times D$ matrix of regression weights and $\Sigma_e$ the residual covariance matrix. Thus, they combine the fields of measurement models and structural analysis. Latent regression models are typically employed to generate plausible values in large-scale assessments.

NOTE THAT USERS REQUIRE THE PIAAC 2012 GERMANY (ZA5845) AND PIAAC-L (ZA5989) SCIENTIFIC-USE-FILES FROM THE RESEARCH DATA CENTRE PIAAC AT GESIS TO WORK WITH R PACKAGE 'PVPIAACL'!

Features
--------

- Independent conjugate prior distributions are chosen to develop a Metropolis-within-Gibbs sampling algorithm based on the device of data augmentation (Tanner & Wong, 1987).
-   Sampling from the posterior distribution of parameters is enriched by sampling from the full conditional distributions of missing values in person covariates.
-   Approximations for the distributions of missing values are constructed from sequential classification and regression trees (Burgette & Reiter, 2010).

Estimation routines
--------

A detailed description of the sample characteristics and the scaling procedures is provided in the technical report on scaling (Carstensen, Gaasch, & Rothaug, 2017).

- PIAAC 2012 competence assessment in literacy, numeracy and problem solving ($D=3$).
- PIAAC 2012 and PIAAC-L 2015 competence assessment in literacy and numeracy ($D=4$).
- PIAAC-L 2015 anchor persons and their partners competence assessment in literacy, numeracy, reading and mathematics ($D=4$).

Installing PVPIAACL
---------------

To install the latest development version from GitHub using the `devtools` package, run:

``` r
if(!require("devtools"))
  install.packages("devtools")
devtools::install_github("jcgaasch/PVPIAACL")
```

Examples
--------



Dependencies
------------

PVPIAACL relies on some routines from other R packages, where the latest CRAN version is in use: `readstata13`, `MASS`, `ucminf`, `rpart` and `Hmisc`.

References
----------

Beguin, A. A., & Glas, C. A. W. (2001). Mcmc estimation and some model-fit analysis of multidimensional irt models. *Psychometrika*, *66*(4), 541-562.

Burgette, L. F., & Reiter, J. P. (2010). Multiple imputation for missing data via sequential regression trees. *American Journal of Epidemiology*, *172*(9), 1070-1076.

Carstensen, C. H., Gaasch, J.-C., & Rothaug, E. (2017). Scaling PIAAC-L cognitive data: technical report. *Manuscript in preparation*.

Tanner, M. A., & Wong, W. H. (1987). The calculation of posterior distributions by data augmentation. *Journal of the American Statistical Association*, *82*(398), 528-549.
