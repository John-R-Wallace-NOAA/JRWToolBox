adjustPolygon <- function (xy, colLine = "red", colPoly = "purple", alpha = 0.25, closePoly = TRUE, lty = 1, 
    colBg = par()$bg, ...) 
{
    lines(xy$x, xy$y, col = colLine, lty = lty, type = "o", ...)
    while (length(PtRow <- identify(xy, labels = "", n = 1)) == 
        1) {
        newPt <- locator(1)
        lines(xy$x, xy$y, lty = lty, col = ifelse(colBg == "transparent", 
            "white", colBg), type = "o", ...)
        xy[PtRow, ] <- newPt
        lines(xy$x, xy$y, lty = lty, col = colLine, type = "o", ...)
    }
    lines(xy$x, xy$y, lty = lty, col = ifelse(colBg == "transparent", 
        "white", colBg), type = "o", ...)
    if (closePoly) {
        xy$x[length(xy$x)] <- xy$x[1]
        xy$y[length(xy$y)] <- xy$y[1]
    }
    polygon(xy$x, xy$y, col = col.alpha(colPoly, alpha), lty = lty, ...)
    invisible(xy)
}
