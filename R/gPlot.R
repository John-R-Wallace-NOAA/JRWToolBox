
 gPlot <- function(data, x.name, y.name, xFunc = NULL, xlab = NULL, ylab = NULL, plotRegionColor = rgb(235, 235, 235, maxColorValue = 255), col = 'black', pch = 1, grid = TRUE, vertLineEachPoint = FALSE, verbose = FALSE, ...) {
    
    if(is.null(xFunc))
         xFunc.name <- NULL
    else      
        xFunc.name <- deparse(substitute(xFunc))
    Formula <- as.formula(paste0(y.name, ' ~ ', xFunc.name, '(', x.name, ')'))
    if(verbose) {
        cat("\n\n")
        print(Formula)
    }     
    plot(Formula, data = data, xlab = ifelse(is.null(xlab), ifelse(is.null(xFunc), x.name, paste0(xFunc.name, '(', x.name, ')')), xlab), ylab = ifelse(is.null(ylab), y.name, ylab), type = "n", ...)
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = plotRegionColor) 
    if(grid)
       grid(col = 'white', lty = 1, lwd = 1)
    if(vertLineEachPoint) 
        abline(v = unique(data[, x.name]), h = 0, col = "grey")
    if(is.null(xFunc))
       points(data[, x.name], data[, y.name], col = col, pch = pch) # Points over grid lines looks better
    else
       points(do.call(xFunc, list(data[, x.name])), data[, y.name], col = col, pch = pch)
}
