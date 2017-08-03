
movePolygon <- function(xy, col = "blue", alpha = 0.25, lty = 1, colBg =  par()$bg, ...) {
    
    polygon(xy$x, xy$y, col = NA, lty = lty, ...)

    while(length(PtRow <- identify(xy, labels = "", n = 1)) == 1) {
	    newPt <- locator(1)
	    polygon(xy$x, xy$y, lty = lty, col = NA, border = ifelse(colBg == "transparent", "white", colBg), ...)
        xy[PtRow,] <- newPt
		polygon(xy$x, xy$y, col = NA, lty = lty, ...)
    }
	polygon(xy$x, xy$y, col = col.alpha(col, alpha), lty = lty, ...)
	invisible(xy)
}

