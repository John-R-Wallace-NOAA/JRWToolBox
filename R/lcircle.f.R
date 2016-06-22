circle.f <- 
function(cx, cy, r, fill.col = NULL, border.col = fill.col, yaxis = T, facets.num = 40, lty.neg = 2, lwd = ifelse(is.null(fill.col), 0, 2), ...)
{
#   cx, cy, coordinates for centre; r is radius
#   yaxis = T, radius is correct on the y axis
#   yaxis = F, radius is correct on the x axis    
#   DATE WRITTEN:  1994      LAST REVISED:   19 Jan 2005
#   AUTHOR:  John R. Wallace (John.Wallace@noaa.gov)
#
	if(r == 0) return(invisible())
	if(r < 0)
		neg.flag <- T
	else neg.flag <- F
	r <- abs(r)
	""
	z <- (seq(0, 360, 360/facets.num) * pi)/180
	pin <- par()$pin
	usr <- par()$usr
	adj <- (pin[2]/pin[1])/((usr[4] - usr[3])/(usr[2] - usr[1]))
	if(yaxis) {
		x <- sin(z) * r * adj + cx
		y <- cos(z) * r + cy
	}
	else {
		x <- sin(z) * r + cx
		y <- (cos(z) * r * 1)/adj + cy
	}
	if(is.null(fill.col)) {
		if(neg.flag)
			lines(x, y, lwd = lwd, lty = lty.neg, ...)
		else llines(x, y, lwd = lwd, ...)
	}
	else {
		polygon(x, y, lty = 1, lwd = lwd, col = fill.col, ...)
		lines(x, y, lty = 1, lwd = lwd, col = border.col)
		if(neg.flag)
			circle.f(cx, cy, r * 0.5, fill.col = 0, yaxis = yaxis, facets.num = facets.num)
	}
	invisible()
}


