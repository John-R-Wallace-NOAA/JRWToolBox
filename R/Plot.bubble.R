Plot.bubble <- function(x, y, z, maxsize = 0.025, num.col = 4, colors = c(2:4, 6:20)[1:num.col], prop.to.area = T, ...)
{
	"Note:  maxsize is proportion to the difference in the range of y"
	""
	xx <- sortmat.f(na.omit(cbind(x, y, z)), 3)
	N <- nrow(xx)
	if(prop.to.area)
		xx[, 3] <- sqrt(xx[, 3])
	maxsize <- maxsize * diff(range(y, na.rm = T))
	xx[, 3] <- (maxsize * xx[, 3])/max(xx[, 3])
	colnums <- colors[cut(xx[, 3], num.col)]
	for(i in 1:N) {
		if(xx[i, 3] == 0)
			points(xx[i, 1], xx[i, 2], pch = 3, mkh = 0.5, ...)
		else circle.f(xx[i, 1], xx[i, 2], xx[i, 3], col = colnums[i], ...)
	}
	invisible()
}
