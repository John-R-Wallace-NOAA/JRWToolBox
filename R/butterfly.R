
butterfly <- function (alpha = 4, beta = 12, plot = TRUE, ...) {

    theta <- seq(0, 24 * pi, len = 2000)
    radius <- exp(cos(theta)) - 2 * cos(round(alpha) * theta) + 
        sin(theta/beta)^5
    x <- -radius * sin(theta)
    y <-  radius * cos(theta)
    if(plot)
       plot(range(x) + c(-0.1, 0.1), range(y) + c(-0.1, 0.1), xlab = "",  
         ylab = "", type = "n", xaxt = "n", yaxt = "n", bty = "n")   
    lines(x, y, ...)
}

Butterfly <- 
function(alpha = 4, beta = 12, ...)
{
	theta <- seq(0, 24 * pi, len = 2000)
	radius <- exp(cos(theta)) - 2 * cos(round(alpha) * theta) + sin(theta/beta)^5
	lines(radius * sin(theta),  - radius * cos(theta), ...)
}

bfly <- 
function(func = JRWToolBox::Butterfly, x.range = c(-4, 4), y.range = c(-5, 5), first.par.limits = c(-10, 10), 
	second.par.limits = c(-5, 5), func.col = NULL, crosshair.col = "blue", canvas = "slategray1", ...)
{
'   '
'   #   DATE WRITTEN:  circa 1990s in Splus     LAST REVISED:  12  March 2022   '
'   #   AUTHOR:  John R. Wallace (John.Wallace@noaa.gov)   '
'   '
    windows.options(canvas = canvas, ...)
    old.loc <- list(x = 4, y = 12)
    plot(x.range, y.range, type = "n")
    func(old.loc$x, old.loc$y, col = 'red')
    par(mar = c(10, 10, 3, 3) + 0.1)
    plot(first.par.limits, second.par.limits, xlab = "", ylab = "", type = "n", cex = 6)
    abline(v = 0, h = 0)
    repeat {
        new.loc <- locator(1)
          if(is.null(new.loc)) break
        func(old.loc$x, old.loc$y, col = canvas)
        abline(h = old.loc$y, col = canvas)
        abline(v = old.loc$x, col = canvas)
        abline(h = new.loc$y, col = crosshair.col, lty = 3)
        abline(v = new.loc$x, col = crosshair.col, lty = 3)
        abline(v = 0, h = 0)
        
        func(new.loc$x, new.loc$y, col = ifelse(is.null(func.col), rainbow(30)[sample(1:30, 1)], func.col))
        old.loc <- new.loc
    }
    invisible()
}

