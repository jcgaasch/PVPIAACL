## Adopted from R package 'bayesm' (Rossi, 2017)
##
## Rossi, P. (2017). bayesm: Bayesian Inference for Marketing/Micro-Econometrics
## [Computer software manual]. Retrieved from
## https://cran.r-project.org/web/packages/bayesm/index.html (version 3.1-0.1)
rwishart <- function(nu, V){

  m <- nrow(V)
  df <- (nu + nu - m + 1) - (nu - m + 1):nu
  T <- diag(sqrt(rchisq(c(rep(1, m)), df)))
  T[lower.tri(T)] <- rnorm((m*(m + 1)/2 - m))
  U <- chol(V)
  C <- t(T)%*%U
  CI <- backsolve(C, diag(m))
  return(crossprod(t(CI)))

}
