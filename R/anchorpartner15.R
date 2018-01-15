#' Plausible values imputation within couples using the PIAAC-L 2015 assessment data in the domains of literacy, numeracy, reading and mathematics
#'
#' This function estimates a four-dimensional latent regression model (first dimension: PIAAC-L 2015 literacy, second dimension:
#' PIAAC-L 2015 numeracy, third dimension: PIAAC-L 2015 reading, fourth dimension: PIAAC-L 2015 mathematics) for binary item response data considering
#' partially missing covariate data. Anchor persons and their partners are treated as one observation. For more detailed information on the statistical model and the estimation algorithm, see the
#' PIAAC-L technical report on scaling (Carstensen, Gaasch & Rothaug, 2017).
#' @param path full path of the folder containing the data files ZA5845 and ZA5989_Persons_15 in Stata format.
#' @param Xanchor data frame containing the permanent person ID (named \code{pnrfestid}) and background variable from the PIAAC and PIAAC-L
#' Scientific Use Files. They can be numeric or factor variables and contain missing values coded as \code{NA}. With \code{Xanchor} set to \code{NULL} (default) no background variables of the anchor person are considered. With both \code{Xanchor} and \code{Xpartner} set to \code{NULL} an empty population model will be estimated.
#' @param Xpartner data frame containing the permanent person ID (named \code{pnrfestid}) and background variable from the PIAAC and PIAAC-L
#' Scientific Use Files. They can be numeric or factor variables and contain missing values coded as \code{NA}. With \code{Xpartner} set to \code{NULL} (default) no background variables of the partner are considered. With both \code{Xanchor} and \code{Xpartner} set to \code{NULL} an empty population model will be estimated.
#' @param nopvs number of plausible values to draw for each respondent.
#' @param itermcmc number of MCMC iterations.
#' @param burnin number of burnin iterations.
#' @return list with \code{nopvs} elements, each containing a data frame of the sequential ID, plausible values for
#' each dimension and, if specified, imputed versions of \code{X}. Additionally each list element is saved as a 
#' Stata file in the folder specified by \code{path}.
#' @references Carstensen, C. H., Gaasch, J.-C., & Rothaug, E. (2017). Scaling PIAAC-L cognitive data: technical report.
#' Manuscript in preparation.
#' @importFrom readstata13 read.dta13 save.dta13
#' @importFrom stats model.matrix runif rnorm pnorm qnorm predict
#' @importFrom MASS mvrnorm
#' @importFrom rpart rpart rpart.control
#' @importFrom Hmisc wtd.mean wtd.var
#' @export
anchorpartner15 <- function(
  path,
  Xanchor = NULL,
  Xpartner = NULL,
  nopvs = 10,
  itermcmc = 22000,
  burnin = 2000
){
  
  t0 <- proc.time()
  files <- list.files(path = path)
  ZA5989_Persons_15 <- suppressWarnings(read.dta13(file = paste0(path,
    files[grep("ZA5989_Persons_15", files)])))
  pnrfestid_anchor <- ZA5989_Persons_15$pnrfestid[which(ZA5989_Persons_15$anchor_15 == "Partner of anchor person") - 1]
  pnrfestid_partner <- ZA5989_Persons_15$pnrfestid[which(ZA5989_Persons_15$anchor_15 == "Partner of anchor person")]
  Lit15 <- ZA5989_Persons_15[which(ZA5989_Persons_15$pnrfestid %in% pnrfestid_anchor),
    c("pnrfestid",
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
  Num15 <- ZA5989_Persons_15[which(ZA5989_Persons_15$pnrfestid %in% pnrfestid_anchor),
    c("pnrfestid",
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
  Reading15 <- ZA5989_Persons_15[which(ZA5989_Persons_15$pnrfestid %in% pnrfestid_partner), 
    c("pnrfestid",
      "REA20110_15", "REA20121_15", "REA20122_15", "REA20123_15", "REA20130_15", "REA20140_15", 
      "REA20151_15", "REA20152_15", "REA20153_15", "REA20210_15", "REA20220_15", "REA20230_15", 
      "REA20240_15", "REA20250_15", "REA20260_15", "REA20270_15", "REA20281_15", "REA20282_15",
      "REA20283_15", "REA20284_15", "REA20285_15", "REA20286_15", "REA20310_15", "REA20320_15", 
      "REA20330_15", "REA20340_15", "REA20350_15", "REA20360_15", "REA20370_15", "REA20381_15", 
      "REA20382_15", "REA20383_15", "REA20410_15", "REA20421_15", "REA20422_15", "REA20430_15",
      "REA20440_15", "REA20450_15", "REA20460_15", "REA20510_15", "REA20521_15", "REA20522_15", 
      "REA20523_15", "REA20530_15", "REA20541_15", "REA20542_15", "REA20543_15", "REA20544_15", 
      "REA20545_15", "REA20550_15")]
  Reading15[, 2:51] <- lapply(Reading15[, 2:51], as.integer)
  Math15 <- ZA5989_Persons_15[which(ZA5989_Persons_15$pnrfestid %in% pnrfestid_partner), 
    c("pnrfestid",
      "MAA2Q071_15", "MAG9V131_15", "MAG9V132_15", "MAG9V133_15", "MAG9V134_15", 
      "MAG9V135_15", "MAG9R261_15", "MAG9R111_15", "MAA2D131_15", "MAA2D132_15", 
      "MAG9R051_15", "MAA2D041_15", "MAA2R081_15", "MAA2V082_15", "MAG9D201_15", 
      "MAA2R091_15", "MAG9V121_15", "MAA2R121_15", "MAA2D111_15", "MAA2R011_15", 
      "MAA2Q101_15", "MAG5V321_15", "MAG9Q021_15", "MAA2V061_15", "MAA2Q021_15")]
  Math15[, 2:26] <- lapply(Math15[, 2:26], as.integer)
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
  Reading15[, -1][Reading15[, -1] == 1] <- NA
  Reading15[, -1][Reading15[, -1] == 2] <- NA
  Reading15[, -1][Reading15[, -1] == 3] <- 1
  Reading15[, -1][Reading15[, -1] == 4] <- 0
  Math15[, -1][Math15[, -1] == 1] <- NA
  Math15[, -1][Math15[, -1] == 2] <- NA
  Math15[, -1][Math15[, -1] == 3] <- 1
  Math15[, -1][Math15[, -1] == 4] <- 0
  LNRM15 <- cbind(pnrfestid_anchor, pnrfestid_partner, Lit15[, -1], Num15[, -1], Reading15[, -1], Math15[, -1])
  LNRM15_valid <- LNRM15[-which(rowSums(is.na(LNRM15)) > 225), ] 
  Y <- data.matrix(LNRM15_valid[, -(1:2)])
  Xid <- LNRM15_valid[, 1:2]
  YOBS <- !is.na(Y)  
  N <- nrow(Y)
  J <- ncol(Y)
  DIM <- 4  
  Jdim <- c(76, 76, 50, 25)
  Jdiminv <- 1/Jdim
  jdim <- list(1:76, 77:152, 153:202, 203:227)
  jdim2 <- c(rep(1, 76), rep(2, 76), rep(3, 50), rep(4, 25))
  YLAT <- matrix(0, nrow = N, ncol = J)
  THETA <- matrix(rnorm(N*DIM), nrow = N, ncol = DIM)
  iterpvs <- sort(sample((burnin + 1):itermcmc, nopvs))
  PVs <- vector("list", nopvs)
  names(PVs) <- paste("Iteration", iterpvs)
  if(!is.null(Xanchor)){
    if(!all(Xid[, 1] %in% Xanchor$pnrfestid)){
      stop(paste0("Input argument Xanchor must contain the respondent having sequential ID ", 
        paste(Xid[which(!(Xidanchor$seqid %in% Xanchor$pnrfestid)), 1], collapse = ", "), "!"))
    }
    X <- merge(Xid, Xanchor, by.x = "pnrfestid_anchor", 
      by.y = "pnrfestid", all.x = TRUE)
    X <- X[, -c(1:2), drop = FALSE]
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
  }else if(!is.null(Xpartner)){
    if(!all(Xid[, 2] %in% Xpartner$pnrfestid)){
      stop(paste0("Input argument Xpartner must contain the respondent having sequential ID ", 
        paste(Xid[which(!(Xipartner$seqid %in% Xpartner$pnrfestid)), 2], collapse = ", "), "!"))
    }
    if(exists("X")){
      X <- merge(X, Xpartner, by.x = "pnrfestid_partner", 
        by.y = "pnrfestid", all.x = TRUE)
    }else{
      X <- merge(Xid, Xpartner, by.x = "pnrfestid_partner", 
        by.y = "pnrfestid", all.x = TRUE)
    }
    X <- X[, -c(1:2), drop = FALSE]
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
  }else{
    ANYXMIS <- FALSE
    XDM <- matrix(1, nrow = N)
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
  CovXi0 <- 100*diag(2)
  PrecXi0 <- solve(CovXi0)
  CovGamma0 <- 100*diag(K1X*DIM)
  PrecGamma0 <- solve(CovGamma0)
  NuSigma <- N + DIM
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
        c("seqid", paste0(c("PVLit", "PVNum", "PVRead", "PVMath"), whichpv,
          rep(c("_anchor", "_partner"), each = 2))))
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
  for(pv in 1:nopvs){
    save.dta13(data = PVs[[pv]], file = paste0(path, "anchorpartner15_", pv, ".dta"))
  }
  t1 <- proc.time()
  ext <- round(round(t1[3] - t0[3]))
  cat("anchorpartner15 completed :)\n")
  if(ext < 7200){
    cat(paste("Your analysis took", round(ext/60),
      "minutes to execute"))
  }else{
    cat(paste("Your analysis took", round(ext/3600, 1),
      "hours to execute"))
  }
  return(PVs)
  
}
