# PVPIAACL: Plausible Values estimation with the PIAAC-L data

The Programme for the International Assessment of Adult Competencies-Longitudinal (PIAAC-L) Germany consortium partner Leibniz Institute for Educational Trajectories developed an R package which implements a Bayesian estimation algorithm that simultaneously generates plausible values (PVs; Mislevy, 1991) and imputes missing values in background variables. In addition to the PVs released in the PIAAC and PIAAC-L Scientific-Use-Files (SUFs), users can estimate PVs themselves specific to their research question, i.e., users select context variables from the PIAAC-L SUFs which are suitable for their analysis and directly enter the population model during PVs estimation. This estimation strategy addresses both item nonresponse in background variables as well as the "curse of dimensionality" due to the extensively large background information resulting from three waves of data collection in PIAAC-L. 

Currently `PVPIAACL` allows the user to fit multidimensional latent regression models. It applies a *D*-dimensional two-parameter normal ogive graded response model without cross-loadings and a multivariate regression equation to model the relationship between the latent trait and additional person covariates. Thus, they combine the fields of measurement models and structural analysis. Latent regression models are typically employed to generate PVs in large-scale assessments.

**Note that users require the PIAAC 2012 Germany (ZA5845) and PIAAC-L (ZA5989) SUFs from the research data centre PIAAC at GESIS to work with R package** `PVPIAACL`.

## Features

- Independent conjugate prior distributions are chosen to develop a Metropolis-within-Gibbs sampling algorithm based on the device of data augmentation (Tanner & Wong, 1987).
-   Sampling from the posterior distribution of parameters is enriched by sampling from the full conditional distributions of missing values in person covariates.
-   Approximations for the distributions of missing values are constructed from sequential classification and regression trees (Burgette & Reiter, 2010; Doove et al., 2014).

## Installation

To install the latest development version from GitHub using the `devtools` package and finally load the package, run:

``` r
if(!require("devtools"))
  install.packages("devtools")
devtools::install_github("jcgaasch/PVPIAACL")
library(PVPIAACL)
```

## Dependencies

PVPIAACL relies on some routines from other R packages where the latest CRAN version is in use: `readstata13`, `MASS`, `mvtnorm`, `ucminf`, `numDeriv`, `rpart` and `Hmisc`.

## Functionality

Aside from the auxiliary functions `lposttau()`, `rwishart()` and `seqcart()` R package `PVPIAACL` offers three main estimation routines: 

- `litnumps12()`: PIAAC 2012 competence assessment in literacy, numeracy and problem solving in technology-rich environments (*D* = 3).
- `litnum1215()`: PIAAC 2012 and PIAAC-L 2015 competence assessment in literacy and numeracy (*D* = 4).
- `anchorpartner15()`: PIAAC-L 2015 anchor persons and their partners competence assessment in literacy, numeracy, reading and mathematics (*D* = 4).

Concerning the test data the estimation routines treat nonresponse, items not reached or not attempted and items missing by design (due to the multiple matrix item sampling design applied in PIAAC) equally as `NA`. Unobserved test data is ignored so that the likelihood is provided only for the observed sample data. A detailed description of the corresponding sample characteristics and scaling procedures is provided in the technical report on scaling (Carstensen, Gaasch, & Rothaug, 2017).

To use any of the functions, users need to create a folder which contains only the original PIAAC and PIAAC-L SUFs ZA5845, ZA5989_Persons_14 and ZA5989_Persons_15 **in Stata format**.

## Examples

### The `litnumps12()` function

PVs for the 2012 PIAAC assessment of the domains of literacy (75 binary and one ordinal items), numeracy (76 binary items) and problem solving (8 binary and six ordinal items) can be estimated using `litnumps12()`. The sample is restricted to realized interviews of anchor persons in PIAAC-L wave one and people with a valid response to at leat two test items (*N* = 3750). The function's usage is

``` r
litnumps12(path, X = NULL, nopvs = 10, itermcmc = 22000, burnin = 2000)
```

The only argument the user (here: myuser) has to specify is `path`, the full path of the folder (here: mydatafolder) created in the step above containing the original PIAAC *and* PIAAC-L SUFs. The default settings

``` r
mypath <- 'C:/Users/myuser/Desktop/mydatafolder/'
PIAAC_PVs_2012_M0 <- litnumps12(path = mypath)
```

will estimate an empty population model. Background variables on the latent competencies both from the PIAAC and PIAAC-L Scientific Use Files can be specified by the argument `X`, where `X` is a data frame containing the sequential ID (which has to be renamed to `seqid`) and the respective covariates. They can be numeric or categorical variables (factors in R) and contain missing values coded as `NA`. **PVPIAACL follows a model-based weighting strategy, i.e, all independent variables used to create weights in PIAAC-L (see the GESIS papers on weighting in PIAAC-L) should be included in the population model**. An exemplary basic specification for `litnumps12()` considering PIAAC background variables from ZA5845 is given as 

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

and can be created in R via 

``` r
library(readstata13)
PIAAC <- read.dta13(file = paste0(mypath, "ZA5845_v2-2-0.dta"))
names(PIAAC)[names(PIAAC) == "SEQID"] <- "seqid"
Xbasic <- PIAAC[, c("seqid", "AGE_R", "GENDER_R", "C_D05", "I_Q08", "J_Q01_C", "J_Q03a",
  "NATIVESPEAKER", "MONTHLYINCPR", "Federal_state", "GKPOL", "PARED", "IMGEN", "EDCAT8")]
```

**Please also note that to avoid uncongeniality problems the conditioning variables included in the latent regression model to generate PVs need to match the variables related to latent abilities in later analyses**. 

Let's say for illustration purposes we want to analyze the relationship of literacy, numeracy and problem solving competencies 2012 with the respondents' grades in German, mathematics and first foreign language surveyed in PIAAC-L wave one. After identifying the variables of interest in the corresponding codebook we need to recode missing values and define levels of measurement (i.e. convert categorical data to `factor`s) before calling `litnumps12()`.  

``` r
library(car)
PIAACL_w1 <- read.dta13(file = paste0(mypath, "ZA5989_Persons_14_v3-0-0.dta"))
X <- PIAACL_w1[, c("seqid", "lsch1_14", "lsch2_14", "lsch3_14")]
X[, -1] <- lapply(X[, -1], 
  function(x){
		x_recode <- recode(x, "c(-3, 7) = NA")
		factor(x_recode)
	}
)
PIAAC_PVs_2012_M1 <- litnumps12(path = mypath, X = X)
```

The return value of the `litnumps12()` is a list with `nopvs` elements (`nopvs` defines the number of PVs to draw for each respondent), each containing a data frame of the sequential ID, PVs for each dimension and, if specified, imputed versions of `X`. Additionally each list element is saved as a Stata file in the folder defined by `path`. Resulting plausible values are transformed onto the PIAAC 2012 scale (weighted means and standard deviations based on the SUF). **Note that PVs and nonresponse imputations have to arise from the same iteration when analyses with plausible values are performed**.

### The `litnum1215()` function

For longitudinal analyses comparing changes between 2015 and 2012 in literacy (one ordinal item recoded and 75 binary items, i.e. 76 binary items at each time point) and numeracy (76 binary items at each time point) competencies use function `litnum1215()`. The sample is restricted to realized interviews of anchor persons in PIAAC-L wave two (*N* = 3263). It can be called in the following manner:

``` r
litnum1215(path, X = NULL, nopvs = 10, itermcmc = 22000, burnin = 2000)
```

The arguments and inputs defined by the user can be specified identically to `litnumps12()`. Also the output created by the function is equal to `litnumps12()`.


### The `anchorpartner15()` function

`PVPIAACL` has function `anchorpartner15()` for making comparisons of similarities and differences in competencies within couples using the PIAAC-L 2015 assessment data in the domains of literacy (one ordinal item recoded and 75 binary items, i.e. 76 binary items), numeracy (76 binary items), reading (50 binary items) and mathematics (25 binary items). Anchor persons and their partners are treated as one observation (i.e. they are listed in one row). The sample is restricted to completed partner interviews of anchor persons in PIAAC-L wave 2 and couples with a valid response to at leat two test items (*N* = 1359). The function's usage is

``` r
anchorpartner15(path, Xanchor = NULL, Xpartner = NULL, nopvs = 10, itermcmc = 22000, burnin = 2000)
```

The arguments and inputs defined by the user can be specified identically to `litnumps12()` except that the permanent person ID (named `pnrfestid`) must be used and two arguments `Xanchor` and `Xpartner` can be specified as data frames holding background variables of anchor persons and their partners respectively. Also the output created by the function is equal to `litnumps12()` except that the resulting plausible values are not linearly transformed onto the PIAAC 2012 scale because the partners were administered only the NEPS instruments for reading and mathematics.


## References

Albert J., & Chib, S. (1997). Bayesian Methods for Cumulative, Sequential and Two-step Ordinal Data Regression Models. Bowling Greene, OH: Department of
Mathematics and Statistics, Bowling Greene State University.

Aßmann, C., Gaasch, J.-C., Pohl, S., & Carstensen C. H. (2015). Bayesian estimation in IRT models with missing values in background variables. *Psychological Test and Assessment Modeling*, *54*(4), 595-618.

Burgette, L. F., & Reiter, J. P. (2010). Multiple imputation for missing data via sequential regression trees. *American Journal of Epidemiology*, *172*(9), 1070-1076.

Carstensen, C. H., Gaasch, J.-C., & Rothaug, E. (2017). Scaling PIAAC-L cognitive data: technical report. *Manuscript in preparation*.

Doove L. L., Van Buuren S., Dusseldorp E. (2014). Recursive Partitioning for Missing Data Imputation in the Presence of Interaction Effects. *Computational Statistics \& Data Analysis* *72*, 92-104.

Edwards, M. C. (2010). A Markov Chain Monte Carlo Approach to Confirmatory Item Factor Analysis. *Psychometrika*, *75*(3), 474-497.

Fox J.-P., Glas C. A. W. (2001). Bayesian estimation of a multilevel irt model using gibbs sampling. *Psychometrika*, *66*(2), 271-288.

Mislevy, R. J. (1991). Randomization-based inference about latent variables from complex samples. *Psychometrika*, *56*(2), 177-196.

Tanner, M. A., & Wong, W. H. (1987). The calculation of posterior distributions by data augmentation. *Journal of the American Statistical Association*, *82*(398), 528-549.
