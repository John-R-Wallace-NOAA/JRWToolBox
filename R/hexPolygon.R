
hexPolygon <- function (x, y, hexC = hexbin::hexcoords(dx, dy, n = 1), dx, dy = NULL, border = 0, ...) 
{
       require(hexbin)
       n <- length(x)
       stopifnot(length(y) == n)
       stopifnot(is.list(hexC) && is.numeric(hexC$x) && is.numeric(hexC$y))
       n7 <- rep.int(7:7, n)
       polygon(x = rep.int(hexC$x, n) + rep.int(x, n7), y = rep.int(hexC$y, n) + rep.int(y, n7), border = border, ...)
    
}
