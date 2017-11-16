panel.Lowess <- function (x, y, span = 0.5, degree = 1, family = c("symmetric", 
    "gaussian"), evaluation = 50, lwd = plot.line$lwd, lty = plot.line$lty, 
    col, col.line = plot.line$col, type, horizontal = FALSE, 
    ..., identifier = "loess") 
{
    x <- as.numeric(x)
    y <- as.numeric(y)
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 1) 
        return()
    if (!missing(col)) {
        if (missing(col.line)) 
            col.line <- col
    }
    plot.line <- trellis.par.get("plot.line")
	# catf('span = ', span, "\n")
	# catf('lwd = ', lwd, "\n")
	# catf('col = ', col, "\n")  # Called as col.line in original function
	# catf('col.line = ', col.line, "\n\n") # plot.line$col
    if (horizontal)
	       JRWToolBox::llowess.line(y[ok], x[ok], smoothing.param = span, lwd = lwd, col=col.line, ...)
    else 
	      JRWToolBox::llowess.line(x[ok], y[ok], smoothing.param = span, lwd = lwd, col=col.line, ...) 
}
