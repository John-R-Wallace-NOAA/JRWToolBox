                      
plot.lowess <- function (x, y, smoothing.param = 2/3, type = "p", jitter.x = FALSE, factor = 1, line.col = "green", 
                method = c("fmm", "periodic", "natural", "monoH.FC", "hyman"), ties = mean, 
                xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), ...) {
                
   if(jitter.x)
      plot(jitter(x, factor), y, type = type, xlab = xlab, ylab = ylab, ...)
   else
      plot(x, y, type = type, xlab = xlab, ylab = ylab, ...)

   JRWToolBox::lowess.line(x, y, smoothing.param = smoothing.param, col = line.col, method = method, ties = ties)
}

                      
plot.lowess.range <- function (x, y, smoothing.param = c(1/6, 1/3, 1/2, 2/3, 5/6), type = "p", jitter.x = FALSE, factor = 1, 
                line.col = rainbow(length(smoothing.param)), method = c("fmm", "periodic", "natural", "monoH.FC", "hyman"), 
                ties = mean, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), ...)   {
                
   if(jitter.x)
      plot(jitter(x, factor), y, type = type, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), ...)
   else
      plot(x, y, type = type, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), ...)

   for (i in 1:length(smoothing.param))
      JRWToolBox::lowess.line(x, y, smoothing.param = smoothing.param[i], col = line.col[i])
}

