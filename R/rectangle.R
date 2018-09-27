rectangle <- function (xleft, ybottom, xright, ytop, ...) {
    ' #  Simplier function than graphics::rect() '
    lines(c(xleft, xleft, xright, xright, xleft), c(ybottom, ytop, ytop, ybottom, ybottom), ...)

}    
 

