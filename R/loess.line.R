
loess.line <- function(x, y, span = 0.75, ...) {

       lo <- stats::loess(y ~ x, span = span)
       j <- order(lo$x)
       lines(lo$x[j], lo$fitted[j], ...)
}
