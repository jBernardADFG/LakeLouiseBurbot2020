#' Function to assess the accuracy criterion for CPUE and abundance
#' 
#' @param ts_mat A matrix specifying the transect/set arrangement. Columns represent transects while rows represent sets. Eg: cbind(c(T,T,T,F),c(T,T,F,F),c(T,T,T,T))
#' @param N_vec A numerical vector specifying the "true" mean abundance
#' @param sig_vec A numerical vector specifying the between transect log scale standard deviation 
#' @param q_bar The estimate of the catchability coefficient
#' @param sd_q An estimate of the standard deviation of the catchability coefficient
#' @param acc_lev A vector of length two giving the prescribed levels of accuracy for CPUE and N respectively.
#' @param A The area of the lake (in XX) 
#' @param n_mc The number of times to run the Monte Carlo simulation
#' @return Returns a matrix giving the percentage of the time that the estimate is within acc_lev percent of the true value. The rows of the matrix coorespond to the entries in N_vec while the columns coorespond to entries in sig_vec.
#' @examples 
#' load("S:/Jordy/louiseOP2020/Data/lakelines.Rdata") # Load Lake Louise lakelines
#' n_max <- 492
#' ts_mat <- pick.coords(lakelines, n_max)$ts_mat
#' N_vec <- seq(1000, 10000, by=1000) 
#' sig_vec <- seq(.1, 1, by=.1)
#' q_bar <- 0.6478999
#' sd_q <- 0.07966257
#' A <- 6519
#' acc_lev <- c(0.3, 0.3)
#' assess.accuracy(ts_mat, N_vec, sig_vec, q_bar, sd_q, acc_lev, A)
#' @export

assess.accuracy <- function(ts_mat, N_vec, sig_vec, q_bar, sd_q, acc_lev, A, n_mc=100){
  n <- ncol(ts_mat)
  m_bar <- mean(apply(ts_mat, MARGIN = 2, sum))
  ret_mat_N <- ret_mat_CPUE <- matrix(NA, length(N_vec), length(sig_vec))
  for(i in 1:length(N_vec)){
    N <- N_vec[i]
    q <- rnorm(1, q_bar, sd_q) # Check sd(q)
    mu <- N/A*q
    for(j in 1:length(sig_vec)){
      sig <- sig_vec[j]
      c_mat <- matrix(NA, nrow(ts_mat), ncol(ts_mat))
      l_1 <- l_2 <- 0
      for (k in 1:n_mc){
        for(t in 1:ncol(c_mat)){
          ep <- rnorm(1, 0, sig)
          gam <- mu/exp(.5*sig^2)*exp(ep)
          for(s in 1:nrow(ts_mat)){
            if (ts_mat[s, t]){
              c_mat[s,t] <- rpois(1, gam)
            }
          }
        }
        cpue_est <- 1/(n*m_bar)*sum(c_mat, na.rm=T)
        acc <- abs(mu-cpue_est)/mu
        if (acc < acc_lev[1]){
          l_1 <- l_1+1
        }
        N_hat <- cpue_est*A/q_bar
        acc <- abs(N-N_hat)/N
        if (acc < acc_lev[2]){
          l_2 <- l_2+1
        }
      }
      ret_mat_CPUE[i,j] <- l_1/n_mc
      ret_mat_N[i,j] <- l_2/n_mc
    }
  }
  rownames(ret_mat_CPUE) <- as.character(N_vec)
  rownames(ret_mat_N) <- as.character(N_vec)
  colnames(ret_mat_CPUE) <- as.character(sig_vec)
  colnames(ret_mat_N) <- as.character(sig_vec)
  r_list <- list(CPUE=ret_mat_CPUE, N=ret_mat_N)
  return(r_list)
}
