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
# ...so using par(pty='s') will eliminate the need for this.
#
#
# Fill is a flag to indicate whether fill in the ellipse.
#
# '...' can pass useful arguments to polygon, such as color 
#     and density.
#
# This is an adjustment by Brad Biggerstaff 
# on 27 April 1999 to the function circle() written
# by John R. Wallace, as noted below.
# 
# The original function was obtained from S-news
#
# Brad Biggerstaff (bkb5@cdc.gov)
# May, 1999
#
# All cos() and sin() switched to make correct on 8 Aug 2017. (JRW)
#
#
#   cx, cy, coordinates for centre; r is radius
#   yaxis = T, radius is correct on the y axis
#   yaxis = F, radius is correct on the x axis    
#   DATE WRITTEN:  1994      LAST REVISED:   08 Aug 2017
#   AUTHOR:  John R. Wallace (John.Wallace@noaa.gov)
#
#
'  # z converted to be in radians  '
    z <- (seq(0, 360, 360/facets.num) * pi)/180
    if(figureAdj) {
       pin <- par()$pin
       usr <- par()$usr
       adj <- (pin[2]/pin[1])/((usr[4] - usr[3])/(usr[2] - usr[1]))
    } else adj <- 1
    if (yaxis) {
        x <- cos(z) * rx * adj
        y <- sin(z) * ry
    }
    else {
        x <- cos(z) * rx
        y <- sin(z) * ry/adj
    }
    xprime <- y * cos(theta) + x * sin(theta) + cx
    yprime <- y * sin(theta) - x * cos(theta) + cy
   
    if (fill) 
       density <- -1
    else density <- 0
    polygon(xprime, yprime, density = density, ...)
    
    invisible(data.frame(x = xprime, y = yprime))

}
