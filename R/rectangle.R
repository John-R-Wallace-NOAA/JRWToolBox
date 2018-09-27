rectangle <- function (xleft, ybottom, xright, ytop, ...) {
    ' #  graphics::rect() doesn't close fully in the upper left-hand corner with lwd = 2 '
    lines(c(xleft, xleft, xright, xright, xleft), c(ybottom, ytop, ytop, ybottom, ybottom), ...)

}    
 

