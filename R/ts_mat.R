# # Script to calculate ts_mat
# # We'll clean this one up later
# get.geo.dist = function(long1, lat1, long2, lat2, units = "m") {
#   longlat1 = purrr::map2(long1, lat1, function(x,y) c(x,y))
#   longlat2 = purrr::map2(long2, lat2, function(x,y) c(x,y))
#   distance_list = purrr::map2(longlat1, longlat2, function(x,y) geosphere::distHaversine(x, y))
#   distance_m = list.extract(distance_list, position = 1)
#   if (units == "km") {
#     distance = distance_m / 1000.0;
#   }
#   else if (units == "miles") {
#     distance = distance_m / 1609.344
#   }
#   else {
#     distance = distance_m
#   }
#   return(distance)
# }
# set.seed(123)
# library(rlist)
# load("S:/Jordy/louiseOP2020/Data/lakelines.Rdata")
# plot(sp)
# x_min <- sp@bbox[1,1]
# x_max <- sp@bbox[1,2]
# y_min <- sp@bbox[2,1]
# y_max <- sp@bbox[2,2]
# get.geo.dist(x_min, y_min, x_max, y_min)/125 # ~ 70
# get.geo.dist(x_min, y_min, x_min, y_max)/125 # ~ 95
# possible_x <- seq(x_min, x_max, length=70)
# possible_y <- seq(y_min, y_max, length=95)
# sample_list <- list()
# n_sets <- 0
# sets <- data.frame()
# n_max <- 491
# library(prevR)
# while(T){
#   tran <- sample(possible_y, 1)
#   possible_y <- possible_y[possible_y!=tran]
#   points <- data.frame(x=possible_x, y=rep(tran,70))
#   new_sets <- points[point.in.SpatialPolygons(points$x, points$y, sp),]
#   if (nrow(sets)+nrow(new_sets) >= n_max){
#     new_sets <- new_sets[1:(n_max-nrow(sets)),]
#     sets <- rbind(sets, new_sets)
#     break
#   }
#   sets <- rbind(sets, new_sets)
# }
# points(sets,col="red")
# tt <-table(sets$y)
# ts_mat <- data.frame(c(rep(T,tt[1]), rep(F, max(tt)-tt[1])))
# for(i in 2:length(tt)){
#   ts_mat <- cbind(ts_mat, c(rep(T,tt[i]), rep(F, max(tt)-tt[i])))
# }
# names(ts_mat) <- NULL
# ts_mat <- as.matrix(ts_mat)
# save(ts_mat, file="S:/Jordy/louiseOP2020/Data/ts_mat.Rdata")
# 
