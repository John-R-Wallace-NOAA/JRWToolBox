panel.xyplot.loess.mspans <- 
function (x, y, type = "p", groups = NULL, pch = if (is.null(groups)) plot.symbol$pch else superpose.symbol$pch, 
    col, col.line = if (is.null(groups)) plot.line$col else superpose.line$col, 
    col.symbol = if (is.null(groups)) plot.symbol$col else superpose.symbol$col, 
    font = if (is.null(groups)) plot.symbol$font else superpose.symbol$font, 
    fontfamily = if (is.null(groups)) plot.symbol$fontfamily else superpose.symbol$fontfamily, 
    fontface = if (is.null(groups)) plot.symbol$fontface else superpose.symbol$fontface, 
    lty = if (is.null(groups)) plot.line$lty else superpose.line$lty, 
    cex = if (is.null(groups)) plot.symbol$cex else superpose.symbol$cex, 
    fill = if (is.null(groups)) plot.symbol$fill else superpose.symbol$fill, 
    lwd = if (is.null(groups)) plot.line$lwd else superpose.line$lwd, 
    horizontal = FALSE, ..., jitter.x = FALSE, jitter.y = FALSE, 
    factor = 0.5, amount = NULL, evaluation = 50, span = 0.75, loess.lwd = 1, loess.points.alpha = 1) 
{

 llowess.line <- function(x, y, smoothing.param = 2/3, ...)
 {
       tmp <- na.omit(cbind(x, y))
       llines(stats::lowess(tmp[, 2]~tmp[, 1], f = smoothing.param), ...)
 }


  all.dots <- list(...)
  printf(all.dots)


    if (all(is.na(x) | is.na(y))) 
        return()
    x <- as.numeric(x)
    y <- as.numeric(y)
    plot.symbol <- trellis.par.get("plot.symbol")
    plot.line <- trellis.par.get("plot.line")
    superpose.symbol <- trellis.par.get("superpose.symbol")
    superpose.line <- trellis.par.get("superpose.line")
    if (!missing(col)) {
        if (missing(col.line)) 
            col.line <- col
        if (missing(col.symbol)) 
            col.symbol <- col
    }
    
 
       x <- as.numeric(x)
       y <- as.numeric(y)
       ok <- is.finite(x) & is.finite(y)
       if (sum(ok) < 1) 
            return()
       if (!missing(col)) {
            if (missing(col.line)) 
               col.line <- col
       }

       if (horizontal) {
           smooth <- loess.smooth(y[ok], x[ok], span = span, family = family, 
               degree = degree, evaluation = evaluation)
           panel.lines(x = smooth$y, y = smooth$x, col = col.line, 
               lty = lty, lwd = lwd, ...)
       }
       else 
          llowess.line(x[ok], y[ok], smoothing.param = span[all.dots$group.number], col = col.line, lwd=loess.lwd)
         
   
}

