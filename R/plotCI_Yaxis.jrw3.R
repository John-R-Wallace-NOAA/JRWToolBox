plotCI_Yaxis.jrw3 <- function (x, y = NULL, uiw, liw = uiw, xlo = NULL, xhi = NULL, 
    sfrac = 0.01, xmax = NULL, xlim = NULL, add = FALSE, maxValue = NULL, 
    col = "black", lwd = 1, x_offset = 0, noNegCI = FALSE, ...) 
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
        ui <- x + uiw
    else ui <- ifelse(x + uiw > maxValue, maxValue, x + uiw)
    li <- x - liw
    if(noNegCI)
        li[li < 0] <- 0
    if (is.null(xlim)) 
        xlim <- range(c(x, ui, li, xlo, xhi, xmax))
    x <- x + x_offset   
    if (!add) 
        plot(x, y, xlim = xlim, col = col, lwd = lwd, ...)
    else points(x, y, col = col, ...)
    smidge <- diff(par("usr")[1:2]) * sfrac
    segments(li, y, ui, y, col = col, lwd = lwd)
    y2 <- c(y, y)
    ul <- c(li, ui)
    segments(ul, y2 - smidge, ul, y2 + smidge, col = col, lwd = lwd)
    invisible(list(x = x, y = y))
}

