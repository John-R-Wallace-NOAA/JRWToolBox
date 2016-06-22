panel.xyplot.loess <- 
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
    if (!is.null(groups)) 
        panel.superpose(x, y, type = type, groups = groups, pch = pch, 
            col.line = col.line, col.symbol = col.symbol, font = font, 
            fontfamily = fontfamily, fontface = fontface, lty = lty, 
            cex = cex, fill = fill, lwd = lwd, horizontal = horizontal, 
            panel.groups = panel.xyplot, jitter.x = jitter.x, 
            jitter.y = jitter.y, factor = factor, amount = amount, 
            ...)
    else {
        if ("o" %in% type || "b" %in% type) 
            type <- c(type, "p", "l")
        if ("g" %in% type) 
            panel.grid(h = -1, v = -1)
        if ("p" %in% type) 
            panel.points(x = if (jitter.x) 
                jitter(x, factor = factor, amount = amount)
            else x, y = if (jitter.y) 
                jitter(y, factor = factor, amount = amount)
            else y, cex = cex, fill = fill, font = font, fontfamily = fontfamily, 
                fontface = fontface, col = col.alpha(col.symbol, alpha = loess.points.alpha), pch = pch, 
                ...)
        if ("l" %in% type) 
            panel.lines(x = x, y = y, lty = lty, col = col.line, 
                lwd = lwd, ...)
        if ("h" %in% type) {
            if (horizontal) 
                panel.lines(x = x, y = y, type = "H", lty = lty, 
                  col = col.line, lwd = lwd, ...)
            else panel.lines(x = x, y = y, type = "h", lty = lty, 
                col = col.line, lwd = lwd, ...)
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
            panel.lines(x = xx, y = yy, lty = lty, col = col.line, 
                lwd = lwd, ...)
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
            panel.lines(x = xx, y = yy, lty = lty, col = col.line, 
                lwd = lwd, ...)
        }
        if ("r" %in% type) 
            panel.lmline(x, y, col = col.line, lty = lty, lwd = lwd, 
                ...)
        if ("smooth" %in% type) 
            panel.loess(x, y, horizontal = horizontal, col = col.line, 
                lty = lty, lwd = lwd, ...)
        if ("a" %in% type) 
            panel.linejoin(x, y, horizontal = horizontal, lwd = lwd, 
                lty = lty, col.line = col.line, ...)
        
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
    else {
        llowess.line(x[ok], y[ok], smoothing.param = span, col = col.line, lwd=loess.lwd)
        if(F) {
        smooth <- loess.smooth(x[ok], y[ok], span = span, family = family, 
            degree = degree, evaluation = evaluation)
        panel.lines(x = smooth$x, y = smooth$y, col = col.line, 
            lty = lty, lwd = lwd, ...)
        }
    }

    }
}

