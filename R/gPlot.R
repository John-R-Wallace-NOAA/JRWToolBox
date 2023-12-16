
gPlot <- function(data, x, y, plotRegionColor = rgb(235, 235, 235, maxColorValue = 255), col = 'black', pch = 1, grid = TRUE, ...) {
   plot(y ~ x, data = data, xlab = "", ylab = "", type = "n", xaxt = "n", yaxt = "n", bty = "n")
   rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = plotRegionColor) 
   if(grid)
      grid(col = 'white', lty = 1, lwd = 1)
   par(new = TRUE)
   # plot(y ~ x, data = data, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), ...) # Does not work ?????????????
   xlab <- deparse(substitute(x)) # Has to saved to an R object to work???
   plot(y ~ x, data = data, xlab = xlab, ylab = deparse(substitute(y)), col = col, pch = pch, ...)
} 
