plotCI_Yaxis.jrw4 <- function (x, y = NULL, uiw, liw = uiw, xlo = NULL, xhi = NULL, 
    sfrac = 0.01, xmax = NULL, xlim = NULL, add = FALSE, maxValue = NULL, 
    col = "black", CI.col = col, lwd = 1, x_offset = 0, lowerLimtCI = NULL, upperLimitCI = NULL, ...) 
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
    if(!is.null(lowerLimtCI))
        li[li < lowerLimtCI] <- lowerLimtCI
    if(!is.null(upperLimitCI))
        ui[ui > upperLimitCI] <- upperLimitCI
    if (is.null(xlim)) 
        xlim <- range(c(x, ui, li, xlo, xhi, xmax))
    x <- x + x_offset   
    if (!add) 
        plot(x, y, xlim = xlim, col = col, lwd = lwd, type = "n", ...)
    smidge <- diff(par("usr")[1:2]) * sfrac
    segments(li, y, ui, y, col = col.alpha(CI.col), lwd = lwd)
    segments(li, y - smidge, li, y + smidge, col = col.alpha(CI.col, 0.2), lwd = lwd)
    segments(ui, y - smidge, ui, y + smidge, col = col.alpha(CI.col, 0.8), lwd = lwd)
    points(x, y, col = col, ...)
    invisible(list(x = x, y = y))
}


