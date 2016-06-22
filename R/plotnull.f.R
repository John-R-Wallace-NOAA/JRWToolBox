plotnull.f<-
function(x = c(0, 1), y = c(0, 1), ...)
{
	plot(x, y, xlab = "", ylab = "", type = "n", xaxt = "n", yaxt = "n", bty = "n", ...)
}

