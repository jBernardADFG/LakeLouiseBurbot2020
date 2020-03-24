#' Function to get the coordinate locations of sets for the 2020 study
#'
#' @param lakelines SpatialPolygons object representing the sampling area
#' @param n_sets The total number of sets
#' @param seed Sets a seed for reproducability
#' @return Prints a map of the selected set locations and returns a list containing information related to the selected transects. $ts_mat will be used in assessAccuracy() and calcPower(); $table and $setcoords will be given to project biologists to position the sets.
#' @examples 
#' load("S:/Jordy/louiseOP2020/Data/lakelines.Rdata") # Load Lake Louise lakelines
#' n_max <- 491
#' pick.coords(lakelines, n_max, quiet=F)
#' @export

pick.coords <- function(lakelines, n_sets, seed=123, quiet=T){
  set.seed(seed)
  x_min <- lakelines@bbox[1,1]
  x_max <- lakelines@bbox[1,2]
  y_min <- lakelines@bbox[2,1]
  y_max <- lakelines@bbox[2,2]
  n_x <- floor(get.geo.dist(x_min, y_min, x_max, y_min)/125)
  n_y <- get.geo.dist(x_min, y_min, x_min, y_max)/125 # ~ 95
  possible_x <- seq(x_min, x_max, length=n_x)
  possible_y <- seq(y_min, y_max, length=n_y)
  sets <- data.frame()
  while(T){
    tran <- sample(possible_y, 1)
    possible_y <- possible_y[possible_y!=tran]
    points <- data.frame(x=possible_x, y=rep(tran,n_x))
    new_sets <- points[point.in.SpatialPolygons(points$x, points$y, lakelines),]
    if (nrow(sets)+nrow(new_sets) >= n_sets){
      new_sets <- new_sets[1:(n_sets-nrow(sets)),]
      sets <- rbind(sets, new_sets)
      break
    }
    sets <- rbind(sets, new_sets)
  }
  tt <-table(sets$y)
  ts_mat <- data.frame(c(rep(T,tt[1]), rep(F, max(tt)-tt[1])))
  for(i in 2:length(tt)){
    ts_mat <- cbind(ts_mat, c(rep(T,tt[i]), rep(F, max(tt)-tt[i])))
  }
  names(ts_mat) <- NULL
  ts_mat <- as.matrix(ts_mat)
  if(!quiet){
    plot(lakelines)
    points(sets, cex=.5, pch=19, col="red")
  }
  return(list(ts_mat=ts_mat, table=tt, set_coords=sets))
}
