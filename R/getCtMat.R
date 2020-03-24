#' Function to read in and prepare the 200 count data
#' 
#' @param file The xlx file to read in. The file should consist of three columns where the first column lists contains the transect #, the second column contains the set within transect #, and the third column contains the fish count. See as.data.frame(read_excel("S:/Jordy/louiseOP2020/Data/CPUE.xls", col_names = F, sheet = "CPUEL"))
#' @param sheet The name of the sheet to read in.
#' @examples
#'  get.ct.mat("S:/Jordy/louiseOP2020/Data/CPUE.xls", "CPUEL")
#' @return Returns a matrix containing the 2005 count data. Note: columns represent transects and rows represent sets within transects.
#' @export
get.ct.mat <- function(file, sheet){
  library(readxl)
  CPUE <- as.data.frame(read_excel(file, col_names = F, sheet = sheet))
  names(CPUE) <- c("transect", "set", "count")
  CPUE <- CPUE[order(CPUE$transect, CPUE$set, CPUE$count),]
  n_trans <- length(levels(as.factor(CPUE$transect)))
  sets_per_trans <- table(CPUE$transect)
  max_n_sets <- max(sets_per_trans)
  ct_mat <- matrix(NA, max_n_sets, n_trans)
  k <- 1
  for (j in 1:n_trans){
    for (i in 1:sets_per_trans[j]){
      ct_mat[i,j] <- CPUE[k,3]
      k <- k+1
    }
  }
  return(ct_mat)
}

