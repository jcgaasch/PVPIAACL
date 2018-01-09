# PVPIAACL: Plausible Values estimation with the PIAAC-L data

The Programme for the International Assessment of Adult Competencies-Longitudinal (PIAAC-L) Germany consortium partner Leibniz Institute for Educational Trajectories developed an R package which implements a Bayesian estimation algorithm that simultaneously generates plausible values (PVs; Mislevy, 1991) and imputes missing values in background variables. In addition to the PVs released in the PIAAC and PIAAC-L Scientific-Use-Files (SUFs), users can estimate PVs themselves specific to their research question, i.e., users select context variables from the PIAAC-L SUFs which are suitable for their analysis and directly enter the population model during PVs estimation. This estimation strategy addresses both item nonresponse in background variables as well as the ``curse of dimensionality'' due to the extensively large background information resulting from three waves of data collection in PIAAC-L. 

Currently `PVPIAACL` allows the user to fit multidimensional latent regression models. It applies a *D*-dimensional two-parameter normal ogive item response theory model (Beguin & Glas, 2001) and a multivariate regression equation to model the relationship between the latent trait and additional person covariates. Thus, they combine the fields of measurement models and structural analysis. Latent regression models are typically employed to generate PVs in large-scale assessments.

**Note that users require the PIAAC 2012 Germany (ZA5845) and PIAAC-L (ZA5989) SUFs from the research data centre PIAAC at GESIS to work with R package** `PVPIAACL`.

## Features

- Independent conjugate prior distributions are chosen to develop a Metropolis-within-Gibbs sampling algorithm based on the device of data augmentation (Tanner & Wong, 1987).
-   Sampling from the posterior distribution of parameters is enriched by sampling from the full conditional distributions of missing values in person covariates.
-   Approximations for the distributions of missing values are constructed from sequential classification and regression trees (Burgette & Reiter, 2010).

## Installation

To install the latest development version from GitHub using the `devtools` package and finally load the package, run:

``` r
if(!require("devtools"))
  install.packages("devtools")
devtools::install_github("jcgaasch/PVPIAACL")
library(PVPIAACL)
```

## Dependencies

PVPIAACL relies on some routines from other R packages, where the latest CRAN version is in use: `readstata13`, `MASS`, `mvtnorm`, `ucminf`, `rpart` and `Hmisc`.

## Functionality

Aside from the auxiliary functions `lposttau()`, `rwishart()` and `seqcart()` R package `PVPIAACL` offers three main estimation routines: 

- `litnumps12()`: PIAAC 2012 competence assessment in literacy, numeracy and problem solving in technology-rich environments (*D* = 3).
- `litnum1215()`: PIAAC 2012 and PIAAC-L 2015 competence assessment in literacy and numeracy (*D* = 4).
- `anchorpartner15()`: PIAAC-L 2015 anchor persons and their partners competence assessment in literacy, numeracy, reading and mathematics (*D* = 4).

A detailed description of the corresponding sample characteristics and scaling procedures is provided in the technical report on scaling (Carstensen, Gaasch, & Rothaug, 2017).

## Examples

In any case users need to create a folder which contains the original PIAAC and PIAAC-L SUFs ZA5845, ZA5989_Persons_14, ZA5989_Household_14, ZA5989_Persons_15, ZA5989_Persons_16 and ZA5989_Household_16 **in Stata format**.

### The `litnumps12()` function

PVs imputation using the PIAAC 2012 assessment data in the domains of literacy, numeracy and problem solving can be conducted via function `litnumps12()`. The function's usage is

``` r
litnumps12(path, XplusVars = NULL, XplusVarsfactor = NULL, nopvs = 10, itermcmc = 22000, burnin = 2000)
```

The only argument the user (here: myuser) has to specify is `path`, the full path of the folder (here: mydatafolder) created in the step above conatining the original PIAAC and PIAAC-L SUFs. The default settings

``` r
PIAAC_PVs_2012 <- litnumps12(path = "C:/Users/myuser/Desktop/mydatafolder/")
```

will estimate the basic specification of the population model considering PIAAC background variables from ZA5845 (see Table 1).

Table 1: Basic specification of the population model for litnumps12() and litnum1215()

| Name          | Label                                                               |
| ------------- |:--------------------------------------------------------------------|
| AGE_R         | Person resolved age from BQ and QC check (derived)                  | 
| GENDER_R      | Person resolved gender from BQ and QC check (derived)               |
| C_D05         | Current status/work history - Employment status (DERIVED BY CAPI)   | 
| I_Q08         | About yourself - Health - State                                     | 
| J_Q01_C       | Background - People in household (top-coded at 6)                   | 
| J_Q03a        | Background - Children                                               | 
| NATIVESPEAKER | Respondent is a native speaker (DERIVED BY CAPI)                    | 
| MONTHLYINCPR  | Monthly income percentile rank category (derived)                   | 
| Federal_state | Federal state - Berlin West/East in one category                    |
| GKPOL         | Political municipality size in 8 categories                         | 
| PARED         | Highest of mother or father's level of education (derived)          | 
| IMGEN         | First and second generation immigrants (derived)                    | 
| EDCAT8        | Highest level of formal education obtained (8 categories - derived) | 

Further background variables either from PIAAC and PIAAC-L can be specified by the arguments `XplusVars` and `XplusVarsfactor`, where `XplusVars` is a character vector containing the additional background variables names and `XplusVarsfactor` is a logical vector indicating which elements of `XplusVars` are categorical variables (factors in R). **Note that to avoid uncongeniality problems the conditioning variables included in the latent regression model to generate PVs need to match the variables related to latent abilities in later analyses**. 

Let's say we want to analyze the relationship of literacy, numeracy and problem solving competencies 2012 with skills used at work surveyed in PIAAC 2012. As described in the corresponding PIAAC codebook the conditioning variables of interest are ordinal scales and thus 

``` r
PIAAC_PVs_2012_skills <- litnumps12(path = "C:/Users/myuser/Desktop/mydatafolder/", 
  XplusVars = c("F_Q02a", "F_Q02b", "F_Q02c", "F_Q02d", "F_Q02e"), XplusVarsfactor = rep(TRUE, 5))
```

will estimate the extended specification of the population model.

The return value of the `litnumps12()` function is a list with `nopvs` elements (`nopvs` defines the number of PVs to draw for each respondent), each containing a data frame of the sequential ID, PVs for each dimension and imputed versions of the partially missing covariate data. Additionally each list element is saved as a Stata file in the folder specified by `path`. **Note that PVs and nonresponse imputations have to arise from the same iteration when analyses with plausible values are performed**.

### The `litnum1215()` function

For longitudinal analyses comparing changes in competences between 2015 and 2012 use function `litnum1215()`. It can be called in the following manner:

``` r
litnum1215(path, XplusVars = NULL, XplusVarsfactor = NULL, nopvs = 10, itermcmc = 22000, burnin = 2000)
```

The arguments and inputs defined by the user can be identically specified to `litnumps12`. Also the output created by the function is equal to `litnumps12`. Thus, it is straightforward to fit the basic and the extended specification of the population model outlined above via 

``` r
PIAAC_PVs_2012_2015 <- litnum1215(path = "C:/Users/myuser/Desktop/mydatafolder/")
PIAAC_PVs_2012_2015_skills <- litnum1215(path = "C:/Users/myuser/Desktop/mydatafolder/", 
  XplusVars = c("F_Q02a", "F_Q02b", "F_Q02c", "F_Q02d", "F_Q02e"), XplusVarsfactor = rep(TRUE, 5))
```

Note that the basic specification for `litnum1215` includes the same background variables as for `litnumps12` (see Table 1).


## References

Beguin, A. A., & Glas, C. A. W. (2001). Mcmc estimation and some model-fit analysis of multidimensional irt models. *Psychometrika*, *66*(4), 541-562.

Burgette, L. F., & Reiter, J. P. (2010). Multiple imputation for missing data via sequential regression trees. *American Journal of Epidemiology*, *172*(9), 1070-1076.

Carstensen, C. H., Gaasch, J.-C., & Rothaug, E. (2017). Scaling PIAAC-L cognitive data: technical report. *Manuscript in preparation*.

Mislevy, R. J. (1991). Randomization-based inference about latent variables from complex samples. *Psychometrika*, *56*(2), 177-196.

Tanner, M. A., & Wong, W. H. (1987). The calculation of posterior distributions by data augmentation. *Journal of the American Statistical Association*, *82*(398), 528-549.
