#' Plausible values imputation using the PIAAC 2012 assessment data in the domains of literacy, numeracy and problem solving
#'
#' This function estimates a three-dimensional latent regression model (first dimension: PIAAC 2012 literacy, second dimension:
#' PIAAC 2012 numeracy, third dimension: PIAAC 2012 problem solving) for binary and ordinal item response data considering
#' partially missing covariate data. For more detailed information on the statistical model and the estimation algorithm, see the
#' PIAAC-L technical report on scaling (Carstensen, Gaasch & Rothaug, 2017).
#' @param path full path of the folder containing the data files ZA5845 and ZA5989_Persons_14 in Stata format.
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
#' @importFrom mvtnorm rmvt dmvt dmvnorm
#' @importFrom ucminf ucminf
#' @importFrom rpart rpart rpart.control
#' @importFrom Hmisc wtd.mean wtd.var
#' @export
litnumps12 <- function(
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
  ZA5989_Persons_14 <- suppressWarnings(read.dta13(file = paste0(path,
    files[grep("ZA5989_Persons_14", files)])))
  names(ZA5845)[names(ZA5845) == "SEQID"] <- "seqid"
  seqid12 <- ZA5989_Persons_14$seqid[!is.na(ZA5989_Persons_14$seqid)]
  Lit12 <- ZA5845[which(ZA5845$seqid %in% seqid12),
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
  Num12 <- ZA5845[which(ZA5845$seqid %in% seqid12),
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
  Ps12 <- ZA5845[which(ZA5845$seqid %in% seqid12),
    c("seqid",
      "U01a000S", "U01b000S", "U02x000S", "U03a000S", "U04a000S", "U06a000S",
      "U06b000S", "U07x000S", "U11b000S", "U16x000S", "U19a000S", "U19b000S",
      "U21x000S", "U23x000S")]
  Ps12[, 2:15] <- lapply(Ps12[, 2:15], as.integer)
  Lit12[, -c(1, 71)][Lit12[, -c(1, 71)] == 1] <- NA
  Lit12[, -c(1, 71)][Lit12[, -c(1, 71)] == 2] <- 1
  Lit12[, -c(1, 71)][Lit12[, -c(1, 71)] == 3] <- 0
  Lit12[, 71][Lit12[, 71] == 1] <- NA
  Lit12[, 71][Lit12[, 71] == 2] <- 1
  Lit12[, 71][Lit12[, 71] == 3] <- 2
  Lit12[, 71][Lit12[, 71] == 4] <- 0
  Num12[, -1][Num12[, -1] == 1] <- NA
  Num12[, -1][Num12[, -1] == 2] <- 1
  Num12[, -1][Num12[, -1] == 3] <- 0
  Ps12[, c(3, 5, 7, 8, 9, 11, 12, 14)][Ps12[, c(3, 5, 7, 8, 9, 11, 12, 14)] == 2] <- 1
  Ps12[, c(3, 5, 7, 8, 9, 11, 12, 14)][Ps12[, c(3, 5, 7, 8, 9, 11, 12, 14)] == 3] <- 0
  Ps12[, c(2, 4, 6, 10, 15)][Ps12[, c(2, 4, 6, 10, 15)] == 1] <- 0
  Ps12[, c(2, 4, 6, 10, 15)][Ps12[, c(2, 4, 6, 10, 15)] == 2] <- 1
  Ps12[, c(2, 4, 6, 10, 15)][Ps12[, c(2, 4, 6, 10, 15)] == 3] <- 2
  Ps12[, c(2, 4, 6, 10, 15)][Ps12[, c(2, 4, 6, 10, 15)] == 4] <- 3
  Ps12[, 13][Ps12[, 13] == 1] <- 0
  Ps12[, 13][Ps12[, 13] == 2] <- 1
  Ps12[, 13][Ps12[, 13] == 3] <- 2
  LN12 <- merge(Lit12, Num12, by = "seqid")
  LNP12 <- merge(LN12, Ps12, by = "seqid")
  LNP12_valid <- LNP12[-which(rowSums(is.na(LNP12[, -1])) > 164), ] 
  Y <- data.matrix(LNP12_valid[, -1])
  Xid <- LNP12_valid[, 1, drop = FALSE]
  YPL1 <- Y + 1
  YPL2 <- Y + 2
  Q <- apply(Y, 2, function(x){
    length(unique(x[!is.na(x)]))
  })
  QMI2 <- Q - 2
  ITEMBIN <- ifelse(Q == 2, TRUE, FALSE)
  POSITEMORD <- which(Q > 2)
  YOBS <- !is.na(Y)  
  N <- nrow(Y)
  J <- ncol(Y)
  DIM <- 3  
  Jdim <- c(76, 76, 14)
  Jdiminv <- 1/Jdim
  jdim <- list(1:76, 77:152, 153:166)
  jdim2 <- c(rep(1, 76), rep(2, 76), rep(3, 14))
  YLAT <- matrix(0, nrow = N, ncol = J)
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
    ALPHA[jdim[[dim]], dim] <- 1
  }
  BETA <- rep(0, J)
  XI <- rbind(rep(1, J), BETA)
  TAU <- lapply(Q, function(x){
    if(x == 2){
      NULL
    }else{
      rep(0, x - 2)
    }
  })
  KAPPA <- lapply(Q, function(x){
    if(x == 2){
      c(-1e+05, 0, 1e+05)
    }else{
      c(-1e+05, 0, cumsum(exp(rep(0, x - 2))), 1e+05)
    }
  })
  CovXi0 <- 100*diag(2)
  PrecXi0 <- solve(CovXi0)
  CovGamma0 <- 100*diag(K1X*DIM)
  PrecGamma0 <- solve(CovGamma0)
  NuSigma <- N + DIM
  ONES <- matrix(1, nrow = N, ncol = 1)
  tdf <- 10
  cat("PVPIAACL is running...\n")
  cat("progress:\n")
  pb <- txtProgressBar(min = 0, max = itermcmc, style = 3)
  for(ii in 1:itermcmc){
    # (1)
    MU <- THETA%*%t(ALPHA) - ONES%*%BETA
    for(j in 1:J){
      FA <- pnorm(KAPPA[[j]][YPL1[, j]] - MU[, j])
      FB <- pnorm(KAPPA[[j]][YPL2[, j]] - MU[, j])
      YLAT[, j] <- MU[, j] + qnorm(runif(N)*(FB - FA) + FA)
    }
    # (2)
    for(dim in 1:DIM){
      XITEM <- cbind(THETA[, dim], -1)
      for(j in jdim[[dim]]){
        Covitem <- solve(crossprod(XITEM[YOBS[, j], ]) + PrecXi0)
        muitem <- Covitem%*%crossprod(XITEM[YOBS[, j], ], YLAT[YOBS[, j], j])
        XI[1, j] <- 0
        while(XI[1, j] <= 0){
          XI[, j] <- mvrnorm(1, muitem, Covitem)
        }
      }
      ALPHA[jdim[[dim]], dim] <- XI[1, jdim[[dim]]]*(1/prod(XI[1, jdim[[dim]]]))^Jdiminv[dim]
      BETA[jdim[[dim]]] <- XI[2, jdim[[dim]]] - sum(XI[2, jdim[[dim]]])/Jdim[dim]
    }
    # (3)
    for(j in POSITEMORD){
      maxTau <- ucminf(par = TAU[[j]], fn = lposttau, Yj = Y[YOBS[, j], j], Qj = QMI2[j],
        alpha = ALPHA[j, jdim2[j]], beta = BETA[j], Theta = THETA[YOBS[, j], jdim2[j]], hessian = 1)
      hatTau <- maxTau$par
      InvhessTau <- solve(maxTau$hessian)
      TAUC <- rmvt(1, delta = hatTau, sigma = InvhessTau, df = tdf)
      ratio <- min(1, exp(
        -lposttau(TAUC, Y[YOBS[, j], j], QMI2[j], ALPHA[j, jdim2[j]], BETA[j], THETA[YOBS[, j], jdim2[j]]) +
        lposttau(TAU[[j]], Y[YOBS[, j], j], QMI2[j], ALPHA[j, jdim2[j]], BETA[j], THETA[YOBS[, j], jdim2[j]]) -
        dmvt(TAUC, delta = hatTau, sigma = InvhessTau, df = tdf, log = TRUE) +
        dmvt(TAU[[j]], delta = hatTau, sigma = InvhessTau, df = tdf, log = TRUE)
      ))
      if(runif(1) < ratio){
        TAU[[j]] <- TAUC
        KAPPA[[j]][3:Q[j]] <- cumsum(exp(TAUC))
      }
    }
    # (4)
    for(i in 1:N){
      Covtheta <- solve(crossprod(ALPHA[YOBS[i, ], ]) + solve(SIGMA))
      mutheta <- Covtheta%*%(crossprod(ALPHA[YOBS[i, ], ], YLAT[i, YOBS[i, ]] +
        BETA[YOBS[i, ]]) + solve(SIGMA)%*%t(GAMMA)%*%XDM[i, ])
      THETA[i, ] <- mvrnorm(1, mutheta, Covtheta)
    }
    # (5)
    Covgamma <- solve(solve(SIGMA)%x%XX + PrecGamma0)
    mugamma <- Covgamma%*%((solve(SIGMA)%x%diag(K1X))%*%matrix(XDMt%*%THETA))
    GAMMA <- matrix(mvrnorm(1, mugamma, Covgamma), nrow = K1X)
    XGAMMA <- XDM%*%GAMMA
    # (6)
    VSigma <- crossprod(THETA - XGAMMA) + diag(DIM)
    SIGMA <- rwishart(NuSigma, chol2inv(chol(VSigma)))
    # save MCMC draws
    saveiter <- ii %in% iterpvs
    if(saveiter){
      whichpv <- which(names(PVs) == paste("Iteration", ii))
      PVs[[whichpv]] <- setNames(data.frame(Xid, THETA),
        c("seqid", paste0(c("PVLit", "PVNum", "PVPs"), whichpv,
          rep("_12", 3))))
    }
    if(ANYXMIS){
      # (7)
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
  PVsPs12_SUF <- Xid
  PVsPs12_SUF <- merge(PVsPs12_SUF,
    ZA5845[, c("seqid", grep("PVPS", names(ZA5845), value = TRUE))],
    by = "seqid")[, -1]  
  GMLit12_SUF <- mean(apply(PVsLit12_SUF, 2, wtd.mean, weights = finalweight12))
  GSdLit12_SUF <- mean(sqrt(apply(PVsLit12_SUF, 2, wtd.var,
    weights = finalweight12)))
  GMNum12_SUF <- mean(apply(PVsNum12_SUF, 2, wtd.mean,
    weights = finalweight12))
  GSdNum12_SUF <- mean(sqrt(apply(PVsNum12_SUF, 2, wtd.var,
    weights = finalweight12)))
  GMPs12_SUF <- mean(apply(PVsPs12_SUF, 2, wtd.mean,
    weights = finalweight12))
  GSdPs12_SUF <- mean(sqrt(apply(PVsPs12_SUF, 2, wtd.var,
    weights = finalweight12)))
  PVsLit12_est <- sapply(PVs, `[[`, 2)
  PVsNum12_est <- sapply(PVs, `[[`, 3)
  PVsPs12_est <- sapply(PVs, `[[`, 4)
  GMLit12_est <- mean(apply(PVsLit12_est, 2, wtd.mean, weights = finalweight12))
  GSdLit12_est <- mean(sqrt(apply(PVsLit12_est, 2, wtd.var, weights = finalweight12)))
  GMNum12_est <- mean(apply(PVsNum12_est, 2, wtd.mean, weights = finalweight12))
  GSdNum12_est <- mean(sqrt(apply(PVsNum12_est, 2, wtd.var, weights = finalweight12)))
  GMPs12_est <- mean(apply(PVsPs12_est, 2, wtd.mean, weights = finalweight12))
  GSdPs12_est <- mean(sqrt(apply(PVsPs12_est, 2, wtd.var, weights = finalweight12)))
  ALit <- GSdLit12_SUF/GSdLit12_est
  BLit <- -GMLit12_est*ALit + GMLit12_SUF
  ANum <- GSdNum12_SUF/GSdNum12_est
  BNum <- -GMNum12_est*ANum + GMNum12_SUF
  APs <- GSdPs12_SUF/GSdPs12_est
  BPs <- -GMPs12_est*APs + GMPs12_SUF
  PVsLit12_est_t <- apply(PVsLit12_est, 2, function(x) x*ALit + BLit)
  PVsNum12_est_t <- apply(PVsNum12_est, 2, function(x) x*ANum + BNum)
  PVsPs12_est_t <- apply(PVsPs12_est, 2, function(x) x*APs + BPs)
  for(pv in 1:nopvs){
    PVs[[pv]][, 2] <- PVsLit12_est_t[, pv]
    PVs[[pv]][, 3] <- PVsNum12_est_t[, pv]
    PVs[[pv]][, 4] <- PVsPs12_est_t[, pv]
    save.dta13(data = PVs[[pv]], file = paste0(path, "litnumps12_", pv, ".dta"))
  }
  t1 <- proc.time()
  ext <- round(round(t1[3] - t0[3]))
  cat("litnumps12 completed :)\n")
  if(ext < 7200){
    cat(paste("Your analysis took", round(ext/60),
      "minutes to execute"))
  }else{
    cat(paste("Your analysis took", round(ext/3600, 1),
      "hours to execute"))
  }
  return(PVs)
  
}
