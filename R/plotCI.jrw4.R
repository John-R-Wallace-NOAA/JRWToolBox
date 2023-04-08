plotCI.jrw4 <- function (x, y = NULL, uiw, liw = uiw, ylo = NULL, yhi = NULL, 
    sfrac = 0.01, ymax = NULL, ylim = NULL, add = FALSE, maxValue = NULL, 
    col = "black", col.seg = "red", lwd = 1, x_offset = 0, ...) 
{
    if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if (is.null(y)) {
        if (is.null(x)) {
            stop("both x and y NULL")
        }
        y <- as.numeric(x)
        x <- seq(along = x)
    }
    if (is.null(maxValue)) 
        ui <- y + uiw
    else ui <- ifelse(y + uiw > maxValue, maxValue, y + uiw)
    li <- y - liw
    if (is.null(ylim)) 
        ylim <- range(c(y, ui, li, ylo, yhi, ymax))
    x <- x + x_offset   
    if (!add) 
        plot(x, y, ylim = ylim, col = col, lwd = lwd, ...)
    else points(x, y, col = col, ...)
    smidge <- diff(par("usr")[1:2]) * sfrac
    segments(x, li, x, ui, col = col, lwd = lwd)
    x2 <- c(x, x)
    ul <- c(li, ui)
    segments(x2 - smidge, ul, x2 + smidge, ul, col = col.seg, lwd = lwd)
    invisible(list(x = x, y = y))
}


