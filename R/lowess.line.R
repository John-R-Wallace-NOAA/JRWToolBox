lowess.line <- function(x, y, smoothing.param = 2/3, ...)
{
       tmp <- na.omit(cbind(x, y))
       lines(stats::lowess(tmp[, 1], tmp[, 2], f = smoothing.param), ...)
} 
