#' Function to calculate sample size requirements for length composition estimates using the methods of Thompson 1987
#' 
#' @param alpha the significance level
#' @param dist the desireed level of precision
#' @return returns the minimum sample size to achieve the desired level of precision at the specified significance level
#' @examples 
#' length.comp.ss(0.05, 0.10) # To estimate the proportions within 10 percentage points of the actual values 95% of the time
#' @export

length.comp.ss <- function(alpha, dist){
  tab <- data.frame(alpha=c(0.50,0.40,0.30,0.20,0.10,0.05,0.025,0.02,0.01), d2n=c(0.44129, 0.50729, 0.60123, 0.74739, 1.00635, 1.27359, 1.55963, 1.65872, 1.96986))
  d2n <- tab[tab$alpha==alpha,2]
  n <- d2n/dist^2
  return(n)
}


