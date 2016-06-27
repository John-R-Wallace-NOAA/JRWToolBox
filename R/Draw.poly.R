Draw.poly <- function (col = 2, lty = 1, ...) 
{
    xy <- locator()
    xy <- data.frame(xy)
    xy <- rbind(xy, xy[1, ])
    polygon(xy$x, xy$y, lty = lty, col = col, ...)
    xy
}
