butterfly <- 
function(alpha = 4, beta = 12, ...)
{
	theta <- seq(0, 24 * pi, len = 2000)
	radius <- exp(cos(theta)) - 2 * cos(round(alpha) * theta) + sin(theta/beta)^5
	lines(radius * sin(theta),  - radius * cos(theta), ...)
}


bfly <- 
function(func = butterfly, x.range = c(-4, 4), y.range = c(-5, 5), first.par.limits = c(-10, 10), 
	second.par.limits = c(-5, 5), func.col = "red", crosshair.col = "blue", canvas = "slategray1", ...)
{

#
#   DATE WRITTEN:  circa 1990's in Splus     LAST REVISED:  02 July 2008
#   AUTHOR:  John R. Wallace (John.Wallace@noaa.gov)
#

    windows.options(canvas = canvas, ...)
    old.loc <- list(x = 4, y = 12)
    dev.set(2)
    plot(x.range, y.range, type = "n")
    func(old.loc$x, old.loc$y, col = func.col)
    dev.set(3)
    par(mar = c(10, 10, 3, 3) + 0.1)
    plot(first.par.limits, second.par.limits, xlab = "", ylab = "", type = "n", cex = 6)
    abline(v = 0, h = 0)
    repeat {
        dev.set(3)
        new.loc <- locator(1)
          if(is.null(new.loc)) break
        abline(h = old.loc$y, col = canvas)
        abline(v = old.loc$x, col = canvas)
        abline(h = new.loc$y, col = crosshair.col, lty = 3)
        abline(v = new.loc$x, col = crosshair.col, lty = 3)
        abline(v = 0, h = 0)
        dev.set(2)
        func(old.loc$x, old.loc$y, col = canvas)
        func(new.loc$x, new.loc$y, col = func.col)
        old.loc <- new.loc
    }
    invisible()
}



