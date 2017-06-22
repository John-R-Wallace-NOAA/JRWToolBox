ellipse <- function(cx, cy, rx, ry = rx, theta = 0, facets.num = 40, yaxis = TRUE, pointsOnly = FALSE, fill = FALSE, ...) {
# Function to plot an ellipse with center (cx,cy)
# and major axis/2 along x equal to rx, 
# major axis/2 along y equal to ry
# and rotated through an angle theta (in radians)
# note that a circle is obtained with rx=ry, 
# in which case theta isn't very helpful either.
#
# The parameter yaxis adjusts to the correct shape in the
# y- or x-axis (FALSE), as plotting is generally not square
# ...so using par(pty="s") will eliminate the need for this.
#
# The parameter 'pointsOnly' determines if a plot is
# added-to (FALSE) or the values of the points are returned
# in a list (TRUE).
#
# Fill is a flag to indicate whether fill in the ellipse.
#
# '...' can pass useful arguments to polygon, such as color 
#     and density.
#
# This is an adjustment by Brad Biggerstaff 
# on 27 April 1999 to the funtion circle() written
# by John R. Wallace, as noted below.
#
# The original function was obtained from S-news
#
# Brad Biggerstaff (bkb5@cdc.gov)
# May, 1999
#
#   cx, cy, coordinates for centre; r is radius
#   yaxis = T, radius is correct on the y axis
#   yaxis = F, radius is correct on the x axis    
#   DATE WRITTEN:  1994      LAST REVISED:   17 July 1995
#   AUTHOR:  John R. Wallace (John.Wallace@noaa.gov)
#
#
	z <- (seq(0, 360, 360/facets.num) * pi)/180
	pin <- par()$pin
	usr <- par()$usr
	adj <- (pin[2]/pin[1])/((usr[4] - usr[3])/(usr[2] - usr[1]))
	if(yaxis) {
		x <- sin(z) * rx * adj
		y <- cos(z) * ry
	}
	else {
		x <- sin(z) * rx
		y <- (cos(z) * ry * 1)/adj
	}
	xprime <- x * cos(theta) + y * sin(theta) + cx
	yprime <- y * cos(theta) - x * sin(theta) + cy
	if(!pointsOnly) {
		if(fill)
			density <- -1
		else density <- 0
		polygon(xprime, yprime, density = density, ...)
		invisible()
	}
	else list(x = xprime, y = yprime)
}
