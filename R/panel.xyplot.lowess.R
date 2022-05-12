panel.xyplot.lowess <- function (x, y, type = "p", groups = NULL, span = 2/3, pch = if (is.null(groups)) plot.symbol$pch else superpose.symbol$pch, 
    col, col.line = if (is.null(groups)) plot.line$col else superpose.line$col, 
    col.symbol = if (is.null(groups)) plot.symbol$col else superpose.symbol$col, 
    font = if (is.null(groups)) plot.symbol$font else superpose.symbol$font, 
    fontfamily = if (is.null(groups)) plot.symbol$fontfamily else superpose.symbol$fontfamily, 
    fontface = if (is.null(groups)) plot.symbol$fontface else superpose.symbol$fontface, 
    lty = if (is.null(groups)) plot.line$lty else superpose.line$lty, 
    cex = if (is.null(groups)) plot.symbol$cex else superpose.symbol$cex, 
    fill = if (is.null(groups)) plot.symbol$fill else superpose.symbol$fill, 
    lwd = if (is.null(groups)) plot.line$lwd else superpose.line$lwd, 
    horizontal = FALSE, ..., grid = FALSE, abline = NULL, jitter.x = FALSE, 
    jitter.y = FALSE, factor = 0.5, amount = NULL, identifier = "xyplot") 
{

# cat("\npanel.xyplot.Loess\n")
# catf('span = ', span, "\n\n")

    if (all(is.na(x) | is.na(y))) 
        return()
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
    if (missing(grid) && ("g" %in% type)) 
        grid <- TRUE
    if (!identical(grid, FALSE)) {
        if (!is.list(grid)) 
            grid <- switch(as.character(grid), `TRUE` = list(h = -1, 
                v = -1, x = x, y = y), h = list(h = -1, v = 0, 
                y = y), v = list(h = 0, v = -1, x = x), list(h = 0, 
                v = 0))
        do.call(panel.grid, grid)
    }
    if (!is.null(abline)) {
        if (is.numeric(abline)) 
            abline <- as.list(abline)
        do.call(panel.abline, abline)
    }
    if (!is.null(groups)) 
        panel.superpose(x, y, type = type, groups = groups, pch = pch, 
            col.line = col.line, col.symbol = col.symbol, font = font, 
            fontfamily = fontfamily, fontface = fontface, lty = lty, 
            cex = cex, fill = fill, lwd = lwd, horizontal = horizontal, 
            panel.groups = function(...) {panel.xyplot.lowess(..., span = span)}, 
            jitter.x = jitter.x, jitter.y = jitter.y, factor = factor, amount = amount, grid = FALSE, ...)
    else {
        x <- as.numeric(x)
        y <- as.numeric(y)
        id <- identifier
        if ("o" %in% type || "b" %in% type) 
            type <- c(type, "p", "l")
        if ("p" %in% type) 
            panel.points(x = if (jitter.x) 
                jitter(x, factor = factor, amount = amount)
            else x, y = if (jitter.y) 
                jitter(y, factor = factor, amount = amount)
            else y, cex = cex, fill = fill, font = font, fontfamily = fontfamily, 
                fontface = fontface, col = col.symbol, pch = pch, ..., identifier = id)
        if ("l" %in% type) 
            panel.lines(x = x, y = y, lty = lty, col = col.line, lwd = lwd, ..., identifier = id)
        if ("h" %in% type) {
            if (horizontal) 
                panel.lines(x = x, y = y, type = "H", lty = lty, col = col.line, lwd = lwd, ..., identifier = id)
            else panel.lines(x = x, y = y, type = "h", lty = lty, col = col.line, lwd = lwd, ..., identifier = id)
        }
        if ("s" %in% type) {
            ord <- if (horizontal) 
                sort.list(y)
            else sort.list(x)
            n <- length(x)
            xx <- numeric(2 * n - 1)
            yy <- numeric(2 * n - 1)
            xx[2 * 1:n - 1] <- x[ord]
            yy[2 * 1:n - 1] <- y[ord]
            xx[2 * 1:(n - 1)] <- x[ord][-1]
            yy[2 * 1:(n - 1)] <- y[ord][-n]
            panel.lines(x = xx, y = yy, lty = lty, col = col.line, lwd = lwd, ..., identifier = id)
        }
        if ("S" %in% type) {
            ord <- if (horizontal) 
                sort.list(y)
            else sort.list(x)
            n <- length(x)
            xx <- numeric(2 * n - 1)
            yy <- numeric(2 * n - 1)
            xx[2 * 1:n - 1] <- x[ord]
            yy[2 * 1:n - 1] <- y[ord]
            xx[2 * 1:(n - 1)] <- x[ord][-n]
            yy[2 * 1:(n - 1)] <- y[ord][-1]
            panel.lines(x = xx, y = yy, lty = lty, col = col.line, lwd = lwd, ..., identifier = id)
        }
        if ("r" %in% type) 
            panel.lmline(x, y, col = col.line, lty = lty, lwd = lwd, ...)
        if ("smooth" %in% type) 
            panel.loess(x, y, horizontal = horizontal, col = col.line, lty = lty, lwd = lwd, ...)    
        if ("lowess" %in% type) {
		# catf("\n\nSmooth\n")
		# catf('span = ', span, "\n\n")
            JRWToolBox::panel.lowess(x, y, horizontal = horizontal, span = span, col.line = col.line, lty = lty, lwd = lwd, ...)
	    }
        if ("spline" %in% type) 
            panel.spline(x, y, horizontal = horizontal, col = col.line, lty = lty, lwd = lwd, ...)
        if ("a" %in% type) 
            panel.linejoin(x, y, horizontal = horizontal, lwd = lwd, lty = lty, col.line = col.line, ...)
    }
}

