inside.xypolygon <- function(pts, polly, test01 = TRUE) {

	verify.xypolygon(pts)
	verify.xypolygon(polly)
	x <- pts$x
	y <- pts$y
	xp <- polly$x
	yp <- polly$y
	npts <- length(x)
	nedges <- length(xp)
	score <- rep(0, npts)
	on.boundary <- rep(FALSE, npts)
	for(i in 1:nedges) {
		x0 <- xp[i]
		y0 <- yp[i]
		x1 <- if(i == nedges) xp[1] else xp[i + 1]
		y1 <- if(i == nedges) yp[1] else yp[i + 1]
		dx <- x1 - x0
		dy <- y1 - y0
		if(dx < 0) {
			xcriterion <- (x - x0) * (x - x1)
			consider <- (xcriterion <= 0)
			if(any(consider)) {
				ycriterion <- y[consider] * dx - x[consider] * dy + (x0 * dy - y0 * dx)
				contrib <- (ycriterion >= 0) * ifelse(xcriterion[consider] == 0, 1/2, 1)
				score[consider] <- score[consider] + contrib
				on.boundary[consider] <- on.boundary[consider] | (ycriterion == 0)
			}
		}
		else if(dx > 0) {
			xcriterion <- (x - x0) * (x - x1)
			consider <- (xcriterion <= 0)
			if(any(consider)) {
				ycriterion <- y[consider] * dx - x[consider] * dy + (x0 * dy - y0 * dx)
				contrib <- (ycriterion < 0) * ifelse(xcriterion[consider] == 0, 1/2, 1)
				score[consider] <- score[consider] - contrib
				on.boundary[consider] <- on.boundary[consider] | (ycriterion == 0)
			}
		}
		else {
			consider <- (x == x0)
			if(any(consider)) {
				yconsider <- y[consider]
				ycriterion <- (yconsider - y0) * (yconsider - y1)
				on.boundary[consider] <- on.boundary[consider] | (ycriterion <= 0)
			}
		}
	}
	score[on.boundary] <- 1
	if(test01) {
		if(!all((score == 0) | (score == 1)))
			warning("internal error: some scores are not equal to 0 or\n 1")
	}
	attr(score, "on.boundary") <- on.boundary
	return(score)
}

 verify.xypolygon <- function(p, fatal = TRUE) {

	whinge <- NULL
	if(!is.list(p) || length(p) != 2 || length(names(p)) != 2 || any(sort(names(p)) != c("x", "y")))
		whinge <- "polygon must be a list with two components x and y"
	else if(is.null(p$x) || is.null(p$y) || !is.numeric(p$x) || !is.numeric(p$y))
		whinge <- "components x and y must be numeric vectors"
	else if(length(p$x) != length(p$y))
		whinge <- "lengths of x and y vectors unequal"
	ok <- is.null(whinge)
	if(!ok && fatal)
		stop(whinge)
	return(ok)
   }




