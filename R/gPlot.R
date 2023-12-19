
gPlot <- function(data, x, y, plotRegionColor = rgb(235, 235, 235, maxColorValue = 255), col = 'black', pch = 1, grid = TRUE, ...) {
   # plot(y ~ x, data = data, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), ...) # Does not work for xlab, but it does for ylab????
   xlab <- deparse(substitute(x)) # xlab has to be saved to an R object to work???
   plot(y ~ x, data = data, xlab = xlab, ylab = deparse(substitute(y)), type = "n", ...)
   rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = plotRegionColor) 
   if(grid)
      grid(col = 'white', lty = 1, lwd = 1)
   points(y ~ x, data = data, col = col, pch = pch) # Points over grid lines looks better
}
