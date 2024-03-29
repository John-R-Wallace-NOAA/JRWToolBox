plotCI.jrw2 <- function (x, y = NULL, ui = NULL, li = NULL, uiw = NULL, liw = NULL, 
    ylo = NULL, yhi = NULL, sfrac = 0.01, ymax = NULL, ylim = NULL, 
    add = FALSE, maxValue = NULL, col = "black", lwd = 1, ...) 
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
    if (is.null(ui)) {
        if (is.null(maxValue)) 
            ui <- y + uiw
        else ui <- ifelse(y + uiw > maxValue, maxValue, y + uiw)
    }
    if (is.null(li)) 
        li <- y - liw
    if (is.null(ylim)) 
        ylim <- range(c(y, ui, li, ylo, yhi, ymax), na.rm = TRUE)
    if (!add) 
        plot(x, y, ylim = ylim, col = col, lwd = lwd, ...)
    else points(x, y, col = col, ...)
    smidge <- diff(par("usr")[1:2]) * sfrac
    segments(x, li, x, ui, col = col, lwd = lwd)
    x2 <- c(x, x)
    ul <- c(li, ui)
    segments(x2 - smidge, ul, x2 + smidge, ul, col = col, lwd = lwd)
    invisible(list(x = x, y = y))
}

