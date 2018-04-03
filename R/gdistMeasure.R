gdistMeasure  <- function(numPts = 2, drawLines = TRUE, lineCol = 'red', pointCol = 'blue', ...) {

    Points <- locator(numPts)
    if(drawLines) {
       lines(Points, lwd=2.0, col=lineCol)
       points(Points, cex=1.5, col=pointCol)
    }
    gdist.total(data.frame(x = Points$x, y = Points$y), ...)
}
