
lowess.line <- function(x, y, smoothing.param = 2/3, method = c("fmm", "periodic", "natural", "monoH.FC", "hyman"), ties = mean, ...) {

       tmp <- na.omit(cbind(x, y))
       lo <- stats::lowess(tmp[, 1], tmp[, 2], f = smoothing.param)
       predictLo <- JRWToolBox::predict.lowess(lo, method = c("fmm", "periodic", "natural", "monoH.FC", "hyman"), ties = mean)
       j <- order(lo$x)
       lines(lo$x[j], predictLo[j], ...)
}
