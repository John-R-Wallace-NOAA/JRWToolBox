
lowess.line <- function(x, y, smoothing.param = 2/3, method = c("fmm", "periodic", "natural", "monoH.FC", "hyman"), ties = mean, ...) {
       
    predict.lowess <- function(loFit, newdata = loFit$x, method = c("fmm", "periodic", "natural", "monoH.FC", "hyman"), ties = mean) { 
      "  "
      "  # dev.new()   "
      "  # JRWToolBox::plot.lowess(cars$speed, cars$dist)   "
  
      "  # lo.car <- lowess(cars$speed, cars$dist)   "
      "  # If the lowess() x variable is descending, then it has to be called explictly, i.e. predict.lowess(lo.car, cars$speed)  "
      "  # points(cars$speed, predict.lowess(lo.car), col = 'dodgerblue', pch = 19)    "
  
      "  # x.new <- c(5.3, 6.8, 20.5, 25.2)   "
      "  # points(x.new, predict.lowess(lo.car, x.new), col = 'red', pch = 19)   "
      "  "
      (stats::splinefun(loFit, method = method, ties = ties))(newdata)
    }
       
    tmp <- na.omit(cbind(x, y))
    lo <- stats::lowess(tmp[, 1], tmp[, 2], f = smoothing.param)
    predictLo <- predict.lowess(lo, method = c("fmm", "periodic", "natural", "monoH.FC", "hyman"), ties = mean)
    j <- order(lo$x)
    lines(lo$x[j], predictLo[j], ...)
}
