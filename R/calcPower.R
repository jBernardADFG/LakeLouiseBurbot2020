#' Power calcualtion function -- determines the power of concluding CPUE has increased from the "base" year
#' 
#' @param ct_mat Count data from the "base" year
#' @param mu_vec A numerical vector specifying the "true" mean CPUE
#' @param sig_vec A numerical vector specifying the between transect log scale standard deviation
#' @param alpha The prescribed level of accuracy
#' @param n_mc The number of times to run the Monte Carlo simulation
#' @return Returns the power of concluding that the cpue has increased since 2005. The rows of the matrix coorespond to the entries in mu_vec while the columns coorespond to entries in sig_vec.
#' @examples 
#' ct_mat <- get.ct.mat("S:/Jordy/louiseOP2020/Data/CPUE.xls", "CPUEL")
#' ct_mat <- ct_mat[,-11]
#' ts_mat <- pick.coords(lakelines, n_max)$ts_mat
#' mu_vec <- seq(0.1, 1, by=.1)
#' sig_vec <- c(0.05, 0.35, 0.85)
#' alpha <- 0.10
#' calc.power(ct_mat, ts_mat, mu_vec, sig_vec, alpha)
#' @export

calc.power <- function(ct_mat, ts_mat, mu_vec, sig_vec, alpha, n_mc=100, quiet=T){
  c <- quantile(CPUE.boot(ct_mat)$CPUE_Bc, (1-alpha))
  ret_mat <- matrix(NA, length(mu_vec), length(sig_vec))
  r <- 0
  for(i in 1:length(mu_vec)){
    mu <- mu_vec[i]
    for(j in 1:length(sig_vec)){
      sig <- sig_vec[j]
      c_mat <- matrix(NA, nrow(ts_mat), ncol(ts_mat))
      l <- 0
      pow <- rep(NA,n_mc)
      for (k in 1:n_mc){
        for(t in 1:ncol(c_mat)){
          ep <- rnorm(1, 0, sig)
          gam <- mu/exp(.5*sig^2)*exp(ep)
          for(s in 1:nrow(c_mat)){
            if (ts_mat[s,t]){
              c_mat[s,t] <- rpois(1, gam)
            }
          }
        }
        samp <- CPUE.boot(c_mat)$CPUE_Bc
        pow[k] <- sum(samp > c)/length(samp)
      }
      if (!quiet){
        r <- r+1
        print(paste(round(r/(length(sig_vec)*length(mu_vec))*100), "percent of the way there"))
      }
      ret_mat[i,j] <- mean(pow) 
    }
  }
  rownames(ret_mat) <- as.character(mu_vec)
  colnames(ret_mat) <- as.character(sig_vec)
  return(ret_mat)
}
