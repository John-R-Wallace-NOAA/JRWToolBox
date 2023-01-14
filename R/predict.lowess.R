
predict.lowess <- function(loFit, newdata = loFit$x, method = c("fmm", "periodic", "natural", "monoH.FC", "hyman"), ties = mean) {  
      "  "
      "  # dev.new()   "
      "  # plot.lowess(cars$speed, cars$dist)   "
  
      "  # lo.car <- lowess(cars$speed, cars$dist)   "
      "  # points(cars$speed, predict.lowess(lo.car), col = 'dodgerblue', pch = 19)    "
  
      "  # x.new <- c(5.3, 6.8, 20.5, 25.2)   "
      "  # points(x.new, predict.lowess(lo.car, x.new), col = 'red', pch = 19)   "
      "  "
   (stats::splinefun(loFit, method = method, ties = ties))(newdata)
}
 
 
