
gPlot <- function(data, x, y, plotRegionColor = rgb(235, 235, 235, maxColorValue = 255), ...) {
   plot(c(0, 1), c(0, 1), xlab = "", ylab = "", type = "n", xaxt = "n", yaxt = "n", bty = "n")
   rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = plotRegionColor) 
   par(new = TRUE)
'  # plot(y ~ x, data = data, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), ...) # Does not work ????  '
'  # Has to be saved to an R object to work!  '
   xlab <- deparse(substitute(x)) 
   plot(y ~ x, data = data, xlab = xlab, ylab = deparse(substitute(y)), ...)
   grid(col = 'white', lty = 1, lwd = 1)
} 
