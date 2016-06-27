panel.superpose.cex <- function (x, y = NULL, subscripts, groups, panel.groups = "panel.xyplot", 
    col = NA, col.line = superpose.line$col, col.symbol = superpose.symbol$col, 
    pch = superpose.symbol$pch, cex = superpose.symbol$cex, fill = superpose.symbol$fill, 
    font = superpose.symbol$font, fontface = superpose.symbol$fontface, 
    fontfamily = superpose.symbol$fontfamily, lty = superpose.line$lty, 
    lwd = superpose.line$lwd, alpha = superpose.symbol$alpha, 
    type = "p", ..., distribute.type = FALSE) 
{
    if (distribute.type) {
        type <- as.list(type)
    }
    else {
        type <- unique(type)
        wg <- match("g", type, nomatch = NA)
        if (!is.na(wg)) {
            panel.grid(h = -1, v = -1)
            type <- type[-wg]
        }
        type <- list(type)
    }
    x <- as.numeric(x)
    if (!is.null(y)) 
        y <- as.numeric(y)
    if (length(x) > 0) {
        if (!missing(col)) {
            if (missing(col.line)) 
                col.line <- col
            if (missing(col.symbol)) 
                col.symbol <- col
        }
        superpose.symbol <- trellis.par.get("superpose.symbol")
        superpose.line <- trellis.par.get("superpose.line")
        vals <- if (is.factor(groups)) 
            levels(groups)
        else sort(unique(groups))
        nvals <- length(vals)
        col <- rep(col, length = nvals)
        col.line <- rep(col.line, length = nvals)
        col.symbol <- rep(col.symbol, length = nvals)
        pch <- rep(pch, length = nvals)
        fill <- rep(fill, length = nvals)
        lty <- rep(lty, length = nvals)
        lwd <- rep(lwd, length = nvals)
        alpha <- rep(alpha, length = nvals)
        "cex <- rep(cex, length = nvals)"
        font <- rep(font, length = nvals)
        fontface <- rep(fontface, length = nvals)
        fontfamily <- rep(fontfamily, length = nvals)
        type <- rep(type, length = nvals)
        panel.groups <- if (is.function(panel.groups)) 
            panel.groups
        else if (is.character(panel.groups)) 
            get(panel.groups)
        else eval(panel.groups)
        subg <- groups[subscripts]
        ok <- !is.na(subg)
        for (i in seq_along(vals)) {
            id <- ok & (subg == vals[i])
            if (any(id)) {
                args <- list(x = x[id], subscripts = subscripts[id], 
                  pch = pch[i], cex = cex[i], font = font[i], 
                  fontface = fontface[i], fontfamily = fontfamily[i], 
                  col = col[i], col.line = col.line[i], col.symbol = col.symbol[i], 
                  fill = fill[i], lty = lty[i], lwd = lwd[i], 
                  alpha = alpha[i], type = type[[i]], group.number = i, 
                  ...)
                if (!is.null(y)) 
                  args$y <- y[id]
                do.call(panel.groups, args)
            }
        }
    }
}

