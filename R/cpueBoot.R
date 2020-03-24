#' For the Rao two stage bootstrap
#' 
#' @param cmat A matrix containing the fish counts. Note: columns represent transects and rows represent sets within transects.
#' @param B The number of bootstrap iterations.
#' @return Returns a list where $CPUE_B is the ordinary bootstrapped sampling distribution and $CPUE_Bc is the bias corrected sampling distribution. 
#' @examples 
#' ct_mat <- get.ct.mat("S:/Jordy/louiseOP2020/Data/CPUE.xls", "CPUEL")
#' ct_mat <- ct_mat[,-11]
#' CPUE.boot(ct_mat)
#' @export

CPUE.boot <- function(cmat, B=1000) {
  n <- ncol(cmat)
  m <- colSums(!is.na(cmat))
  CPUEbar <- mean(cmat, na.rm=T)
  outB <- outBc <- rep(NA, B)
  allcols <- sample(1:n, n*B, replace=T)
  for(k in 1:B) {
    cols <- allcols[(n*(k-1)+1):(k*n)] 
    kmat <- cmat[,cols]
    mkmat <- m[cols] 
    cmatB <- NA*cmat
    for(j in 1:n) {
      cmatB[1:mkmat[j], j] <- sample(kmat[1:mkmat[j], j], mkmat[j], replace=T)
    }
    mistar <- mkmat 
    wistar <- mistar/mean(mistar)
    cistar <- colMeans(cmatB, na.rm=T)
    cmatBc <- t(CPUEbar + sqrt(n/(n-1))*(wistar*cistar - CPUEbar) + wistar*sqrt(mistar/(mistar-1))*(t(cmatB)-cistar))
    outB[k] <- mean(cmatB, na.rm=T)
    outBc[k] <- mean(colMeans(cmatBc, na.rm=T))
  }
  out <- list(CPUE_B=outB, CPUE_Bc=outBc)
  return(out)
}
