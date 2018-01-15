#' Plausible values imputation using the PIAAC 2012 and PIAAC-L 2015 assessment data in the domains of literacy and numeracy
#'
#' This function estimates a four-dimensional latent regression item response model (first dimension: PIAAC 2012 literacy,
#' second dimension: PIAAC 2012 numeracy, third dimension: PIAAC-L 2015 literacy, fourth dimension: PIAAC-L 2015 numeracy)
#' for binary item response data considering partially missing covariate data. For more detailed information on the statistical
#' model and the estimation algorithm, see the PIAAC-L technical report on scaling (Carstensen, Gaasch & Rothaug, 2017).
#' @param path full path of the folder containing the data files ZA5845 and ZA5989_Persons_15 in Stata format.
#' @param X data frame containing the sequential ID (named \code{seqid}) and background variable from the PIAAC and PIAAC-L
#' Scientific Use Files. They can be numeric or factor variables and contain missing values coded as \code{NA}. With \code{X} set to \code{NULL} (default) an empty population model is estimated.
#' @param nopvs number of plausible values to draw for each respondent.
#' @param itermcmc number of MCMC iterations.
#' @param burnin number of burnin iterations.
#' @return list with \code{nopvs} elements, each containing a data frame of the sequential ID, plausible values for
#' each dimension and, if specified, imputed versions of \code{X}. Resulting plausible values are transformed onto the PIAAC 2012 scale (weighted means and standard deviations based on the SUF). Additionally each list element is saved as a 
#' Stata file in the folder specified by \code{path}.
#' @references Carstensen, C. H., Gaasch, J.-C., & Rothaug, E. (2017). Scaling PIAAC-L cognitive data: technical report.
#' Manuscript in preparation.
#' @importFrom readstata13 read.dta13 save.dta13
#' @importFrom stats model.matrix runif rnorm pnorm qnorm predict
#' @importFrom MASS mvrnorm
#' @importFrom rpart rpart rpart.control
#' @importFrom Hmisc wtd.mean wtd.var
#' @export
litnum1215 <- function(
  path,
  X = NULL,
  nopvs = 10,
  itermcmc = 22000,
  burnin = 2000
){
  
  t0 <- proc.time()
  files <- list.files(path = path)
  ZA5845 <- suppressWarnings(read.dta13(file = paste0(path,
    files[grep("ZA5845", files)])))
  ZA5989_Persons_15 <- suppressWarnings(read.dta13(file = paste0(path,
    files[grep("ZA5989_Persons_15", files)])))
  names(ZA5845)[names(ZA5845) == "SEQID"] <- "seqid"
  seqid1215 <- ZA5989_Persons_15$seqid[!is.na(ZA5989_Persons_15$seqid)]
  Lit12 <- ZA5845[which(ZA5845$seqid %in% seqid1215),
    c("seqid",
      "C301C05S", "C300C02S", "D302C02S", "D311701S", "C308120S", "E321001S",
      "E321002S", "C305215S", "C305218S", "C308117S", "C308119S", "C308121S",
      "C308118S", "D304710S", "D304711S", "D315512S", "E327001S", "E327002S",
      "E327003S", "E327004S", "C308116S", "C309320S", "C309321S", "D307401S",
      "D307402S", "C313412S", "C313414S", "C309319S", "C309322S", "E322001S",
      "E322002S", "E322005S", "E320001S", "E320003S", "E320004S", "C310406S",
      "C310407S", "E322003S", "E323003S", "E323004S", "E322004S", "D306110S",
      "D306111S", "C313410S", "C313411S", "C313413S", "E318001S", "E318003S",
      "E323002S", "E323005S", "E329002S", "E329003S", "M301C05S", "P330001S",
      "N302C02S", "M300C02S", "N306110S", "N306111S", "M313410S", "M313411S",
      "M313412S", "M313413S", "M313414S", "P324002S", "P324003S", "M305215S",
      "M305218S", "P317001S", "P317002S", "P317003S", "M310406S", "M310407S",
      "M309319S", "M309320S", "M309321S", "M309322S")]
  Lit12[, 2:77] <- lapply(Lit12[, 2:77], as.integer)
  Num12 <- ZA5845[which(ZA5845$seqid %in% seqid1215),
    c("seqid",
      "C600C04S", "C601C06S", "E645001S", "C615602S", "C615603S", "C624619S",
      "C624620S", "C604505S", "C605506S", "C605507S", "C605508S", "E650001S",
      "C623616S", "C623617S", "C619609S", "E657001S", "E646002S", "C620610S",
      "C620612S", "E632001S", "E632002S", "C607510S", "C614601S", "C618607S",
      "C618608S", "E635001S", "C613520S", "C608513S", "E655001S", "C602501S",
      "C602502S", "C602503S", "C611516S", "C611517S", "C606509S", "E665001S",
      "E665002S", "C622615S", "E636001S", "C617605S", "C617606S", "E641001S",
      "E661001S", "E661002S", "E660003S", "E660004S", "E634001S", "E634002S",
      "E651002S", "E664001S", "E644002S", "C612518S", "M600C04S", "P601C06S",
      "P614601S", "P645001S", "M615602S", "M615603S", "P640001S", "M620610S",
      "M620612S", "P666001S", "M623616S", "M623617S", "M623618S", "M624619S",
      "M624620S", "M618607S", "M618608S", "M604505S", "M610515S", "P664001S",
      "M602501S", "M602502S", "M602503S", "P655001S")]
  Num12[, 2:77] <- lapply(Num12[, 2:77], as.integer)
  Lit15 <- ZA5989_Persons_15[which(ZA5989_Persons_15$seqid %in% seqid1215),
    c("seqid",
      "C301C05S_15", "C300C02S_15", "D302C02S_15", "D311701S_15", "C308120S_15",
      "E321001S_15", "E321002S_15", "C305215S_15", "C305218S_15", "C308117S_15",
      "C308119S_15", "C308121S_15", "C308118S_15", "D304710S_15", "D304711S_15",
      "D315512S_15", "E327001S_15", "E327002S_15", "E327003S_15", "E327004S_15",
      "C308116S_15", "C309320S_15", "C309321S_15", "D307401S_15", "D307402S_15",
      "C313412S_15", "C313414S_15", "C309319S_15", "C309322S_15", "E322001S_15",
      "E322002S_15", "E322005S_15", "E320001S_15", "E320003S_15", "E320004S_15",
      "C310406S_15", "C310407S_15", "E322003S_15", "E323003S_15", "E323004S_15",
      "E322004S_15", "D306110S_15", "D306111S_15", "C313410S_15", "C313411S_15",
      "C313413S_15", "E318001S_15", "E318003S_15", "E323002S_15", "E323005S_15",
      "E329002S_15", "E329003S_15", "M301C05S_15", "P330001S_15", "N302C02S_15",
      "M300C02S_15", "N306110S_15", "N306111S_15", "M313410S_15", "M313411S_15",
      "M313412S_15", "M313413S_15", "M313414S_15", "P324002S_15", "P324003S_15",
      "M305215S_15", "M305218S_15", "P317001S_15", "P317002S_15", "P317003S_15",
      "M310406S_15", "M310407S_15", "M309319S_15", "M309320S_15", "M309321S_15",
      "M309322S_15")]
  Lit15[, 2:77] <- lapply(Lit15[, 2:77], as.integer)
  Num15 <- ZA5989_Persons_15[which(ZA5989_Persons_15$seqid %in% seqid1215),
    c("seqid",
      "C600C04S_15", "C601C06S_15", "E645001S_15", "C615602S_15", "C615603S_15",
      "C624619S_15", "C624620S_15", "C604505S_15", "C605506S_15", "C605507S_15",
      "C605508S_15", "E650001S_15", "C623616S_15", "C623617S_15", "C619609S_15",
      "E657001S_15", "E646002S_15", "C620610S_15", "C620612S_15", "E632001S_15",
      "E632002S_15", "C607510S_15", "C614601S_15", "C618607S_15", "C618608S_15",
      "E635001S_15", "C613520S_15", "C608513S_15", "E655001S_15", "C602501S_15",
      "C602502S_15", "C602503S_15", "C611516S_15", "C611517S_15", "C606509S_15",
      "E665001S_15", "E665002S_15", "C622615S_15", "E636001S_15", "C617605S_15",
      "C617606S_15", "E641001S_15", "E661001S_15", "E661002S_15", "E660003S_15",
      "E660004S_15", "E634001S_15", "E634002S_15", "E651002S_15", "E664001S_15",
      "E644002S_15", "C612518S_15", "M600C04S_15", "P601C06S_15", "P614601S_15",
      "P645001S_15", "M615602S_15", "M615603S_15", "P640001S_15", "M620610S_15",
      "M620612S_15", "P666001S_15", "M623616S_15", "M623617S_15", "M623618S_15",
      "M624619S_15", "M624620S_15", "M618607S_15", "M618608S_15", "M604505S_15",
      "M610515S_15", "P664001S_15", "M602501S_15", "M602502S_15", "M602503S_15",
      "P655001S_15")]
  Num15[, 2:77] <- lapply(Num15[, 2:77], as.integer)
  Lit12[, -c(1, 71)][Lit12[, -c(1, 71)] == 1] <- NA
  Lit12[, -c(1, 71)][Lit12[, -c(1, 71)] == 2] <- 1
  Lit12[, -c(1, 71)][Lit12[, -c(1, 71)] == 3] <- 0
  Lit12[, 71][Lit12[, 71] == 1] <- NA
  Lit12[, 71][Lit12[, 71] %in% c(2, 3)] <- 1
  Lit12[, 71][Lit12[, 71] == 4] <- 0
  Num12[, -1][Num12[, -1] == 1] <- NA
  Num12[, -1][Num12[, -1] == 2] <- 1
  Num12[, -1][Num12[, -1] == 3] <- 0
  Lit15[, -c(1, 71)][Lit15[, -c(1, 71)] == 2] <- NA
  Lit15[, -c(1, 71)][Lit15[, -c(1, 71)] == 3] <- NA
  Lit15[, -c(1, 71)][Lit15[, -c(1, 71)] == 4] <- 1
  Lit15[, -c(1, 71)][Lit15[, -c(1, 71)] == 5] <- 0
  Lit15[, 71][Lit15[, 71] %in% c(2, 3)] <- NA
  Lit15[, 71][Lit15[, 71] %in% c(4, 5)] <- 1
  Lit15[, 71][Lit15[, 71] == 6] <- 0
  Num15[, -1][Num15[, -1] == 2] <- NA
  Num15[, -1][Num15[, -1] == 3] <- NA
  Num15[, -1][Num15[, -1] == 4] <- 1
  Num15[, -1][Num15[, -1] == 5] <- 0
  LN12 <- merge(Lit12, Num12, by = "seqid")
  LN15 <- merge(Lit15, Num15, by = "seqid")
  LN1215 <- merge(LN12, LN15, by = "seqid")
  Y <- data.matrix(LN1215[, -1])
  Xid <- LN1215[, 1, drop = FALSE]
  YOBS <- !is.na(Y)
  N <- nrow(Y)
  J <- ncol(Y)
  DIM <- 4
  J1dim <- J/DIM
  J1diminv <- 1/J1dim
  Jdim <- matrix(1:J, nrow = J1dim)
  THETA <- matrix(rnorm(N*DIM), nrow = N, ncol = DIM)
  iterpvs <- sort(sample((burnin + 1):itermcmc, nopvs))
  PVs <- vector("list", nopvs)
  names(PVs) <- paste("Iteration", iterpvs)
  if(is.null(X)){
    ANYXMIS <- FALSE
    XDM <- matrix(1, nrow = N)
  }else{
    if(!all(Xid$seqid %in% X$seqid)){
      stop(paste0("Input argument X must contain the respondent having sequential ID ", 
        paste(Xid$seqid[which(!(Xid$seqid %in% X$seqid))], collapse = ", "), "!"))
    }
    X <- merge(Xid, X, by = "seqid", all.x = TRUE)
    X <- X[, -1, drop = FALSE]
    ANYXMIS <- any(is.na(X))
    if(ANYXMIS){
      XOBS <- !is.na(X)
      XMIS <- is.na(X)
      xmisord <- names(sort(colSums(XMIS)))[sort(colSums(XMIS)) > 0]
      for(k in xmisord){
        X[XMIS[, k], k] <- sample(X[XOBS[, k], k], sum(XMIS[, k]), replace = TRUE)
      }
      X2IMP <- data.frame(X, THETA)
      XcolsTHETA <- (ncol(X2IMP) - DIM + 1):ncol(X2IMP)
    }
    XDM <- model.matrix(~., X)
  }
  K1X <- ncol(XDM)
  XDMt <- t(XDM)
  XX <- crossprod(XDM)
  GAMMA <- matrix(rep(0, K1X*DIM), nrow = K1X)
  XGAMMA <- XDM%*%GAMMA
  SIGMA <- diag(DIM)
  ALPHA <- matrix(0, nrow = J, ncol = DIM)
  for(dim in 1:DIM){
    ALPHA[Jdim[, dim], dim] <- 1
  }
  BETA <- rep(0, J)
  XI <- rbind(rep(1, J), BETA)
  CovXi0 <- 100*diag(2)
  PrecXi0 <- solve(CovXi0)
  CovGamma0 <- 100*diag(K1X*DIM)
  PrecGamma0 <- solve(CovGamma0)
  NuSigma <- N + DIM
  LO <- matrix(c(-Inf, 0)[Y + 1], N, J)
  HI <- matrix(c(0, Inf)[Y + 1], N, J)
  ONES <- matrix(1, nrow = N, ncol = 1)
  cat("PVPIAACL is running...\n")
  cat("progress:\n")
  pb <- txtProgressBar(min = 0, max = itermcmc, style = 3)
  for(ii in 1:itermcmc){
    # (1)
    MU <- THETA%*%t(ALPHA) - ONES%*%BETA
    FA <- pnorm(LO - MU)
    FB <- pnorm(HI - MU)
    YLAT <- MU + qnorm(matrix(runif(N*J), nrow = N, ncol = J)*(FB - FA) + FA)
    # (2)
    for(dim in 1:DIM){
      XITEM <- cbind(THETA[, dim], -1)
      for(j in Jdim[, dim]){
        Covitem <- solve(crossprod(XITEM[YOBS[, j], ]) + PrecXi0)
        muitem <- Covitem%*%crossprod(XITEM[YOBS[, j], ], YLAT[YOBS[, j], j])
        XI[1, j] <- 0
        while(XI[1, j] <= 0){
          XI[, j] <- mvrnorm(1, muitem, Covitem)
        }
      }
    }
    ALPHALITMEAN <- (XI[1, Jdim[, 1]] + XI[1, Jdim[, 3]])/2
    ALPHANUMMEAN <- (XI[1, Jdim[, 2]] + XI[1, Jdim[, 4]])/2
    BETALITMEAN <- (XI[2, Jdim[, 1]] + XI[2, Jdim[, 3]])/2
    BETANUMMEAN <- (XI[2, Jdim[, 2]] + XI[2, Jdim[, 4]])/2
    ALPHALITR <- ALPHALITMEAN*(1/prod(ALPHALITMEAN))^J1diminv
    ALPHANUMR <- ALPHANUMMEAN*(1/prod(ALPHANUMMEAN))^J1diminv
    BETALITR <- BETALITMEAN - sum(BETALITMEAN)/J1dim
    BETANUMR <- BETANUMMEAN - sum(BETANUMMEAN)/J1dim
    ALPHA[Jdim[, 1], 1] <- ALPHA[Jdim[, 3], 3] <- ALPHALITR
    ALPHA[Jdim[, 2], 2] <- ALPHA[Jdim[, 4], 4] <- ALPHANUMR
    BETA[c(Jdim[, 1], Jdim[, 3])] <- BETALITR
    BETA[c(Jdim[, 2], Jdim[, 4])] <- BETANUMR
    # (3)
    for(i in 1:N){
      Covtheta <- solve(crossprod(ALPHA[YOBS[i, ], ]) + solve(SIGMA))
      mutheta <- Covtheta%*%(crossprod(ALPHA[YOBS[i, ], ], YLAT[i, YOBS[i, ]] +
        BETA[YOBS[i, ]]) + solve(SIGMA)%*%t(GAMMA)%*%XDM[i, ])
      THETA[i, ] <- mvrnorm(1, mutheta, Covtheta)
    }
    # (4)
    Covgamma <- solve(solve(SIGMA)%x%XX + PrecGamma0)
    mugamma <- Covgamma%*%((solve(SIGMA)%x%diag(K1X))%*%matrix(XDMt%*%THETA))
    GAMMA <- matrix(mvrnorm(1, mugamma, Covgamma), nrow = K1X)
    XGAMMA <- XDM%*%GAMMA
    # (5)
    VSigma <- crossprod(THETA - XGAMMA) + diag(DIM)
    SIGMA <- rwishart(NuSigma, chol2inv(chol(VSigma)))
    # save MCMC draws
    saveiter <- ii %in% iterpvs
    if(saveiter){
      whichpv <- which(names(PVs) == paste("Iteration", ii))
      PVs[[whichpv]] <- setNames(data.frame(Xid, THETA),
        c("seqid", paste0(rep(c("PVLit", "PVNum"), 2), whichpv,
          rep(c("_12", "_15"), each = 2))))
    }
    if(ANYXMIS){
      # (6)
      X2IMP[, XcolsTHETA] <- THETA
      X <- seqcart(X2IMP, xmisord, XOBS, XMIS, 5, 1e-04)
      X <- X[, -XcolsTHETA]
      XDM <- model.matrix(~., X)
      XDMt <- t(XDM)
      XX <- crossprod(XDM)
      if(saveiter){
        PVs[[whichpv]] <- cbind(PVs[[whichpv]], X)
      }
    }
    setTxtProgressBar(pb, ii)
  }
  close(pb)
  finalweight12 <- Xid
  finalweight12 <- merge(finalweight12,
    ZA5845[, c("seqid", "SPFWT0")], by = "seqid")[, -1]
  PVsLit12_SUF <- Xid
  PVsLit12_SUF <- merge(PVsLit12_SUF,
    ZA5845[, c("seqid", grep("PVLIT", names(ZA5845), value = TRUE))],
    by = "seqid")[, -1]
  PVsNum12_SUF <- Xid
  PVsNum12_SUF <- merge(PVsNum12_SUF,
    ZA5845[, c("seqid", grep("PVNUM", names(ZA5845), value = TRUE))],
    by = "seqid")[, -1]
  GMLit12_SUF <- mean(apply(PVsLit12_SUF, 2, wtd.mean, weights = finalweight12))
  GSdLit12_SUF <- mean(sqrt(apply(PVsLit12_SUF, 2, wtd.var,
    weights = finalweight12)))
  GMNum12_SUF <- mean(apply(PVsNum12_SUF, 2, wtd.mean,
    weights = finalweight12))
  GSdNum12_SUF <- mean(sqrt(apply(PVsNum12_SUF, 2, wtd.var,
    weights = finalweight12)))
  PVsLit12_est <- sapply(PVs, `[[`, 2)
  PVsNum12_est <- sapply(PVs, `[[`, 3)
  PVsLit15_est <- sapply(PVs, `[[`, 4)
  PVsNum15_est <- sapply(PVs, `[[`, 5)
  GMLit12_est <- mean(apply(PVsLit12_est, 2, wtd.mean, weights = finalweight12))
  GSdLit12_est <- mean(sqrt(apply(PVsLit12_est, 2, wtd.var, weights = finalweight12)))
  GMNum12_est <- mean(apply(PVsNum12_est, 2, wtd.mean, weights = finalweight12))
  GSdNum12_est <- mean(sqrt(apply(PVsNum12_est, 2, wtd.var, weights = finalweight12)))
  ALit <- GSdLit12_SUF/GSdLit12_est
  BLit <- -GMLit12_est*ALit + GMLit12_SUF
  ANum <- GSdNum12_SUF/GSdNum12_est
  BNum <- -GMNum12_est*ANum + GMNum12_SUF
  PVsLit12_est_t <- apply(PVsLit12_est, 2, function(x) x*ALit + BLit)
  PVsNum12_est_t <- apply(PVsNum12_est, 2, function(x) x*ANum + BNum)
  PVsLit15_est_t <- apply(PVsLit15_est, 2, function(x) x*ALit + BLit)
  PVsNum15_est_t <- apply(PVsNum15_est, 2, function(x) x*ANum + BNum)
  for(pv in 1:nopvs){
    PVs[[pv]][, 2] <- PVsLit12_est_t[, pv]
    PVs[[pv]][, 3] <- PVsNum12_est_t[, pv]
    PVs[[pv]][, 4] <- PVsLit15_est_t[, pv]
    PVs[[pv]][, 5] <- PVsNum15_est_t[, pv]
    save.dta13(data = PVs[[pv]], file = paste0(path, "litnum1215_", pv, ".dta"))
  }
  t1 <- proc.time()
  ext <- round(round(t1[3] - t0[3]))
  cat("litnum1215 completed :)\n")
  if(ext < 7200){
    cat(paste("Your analysis took", round(ext/60),
      "minutes to execute"))
  }else{
    cat(paste("Your analysis took", round(ext/3600, 1),
      "hours to execute"))
  }
  return(PVs)
  
}
