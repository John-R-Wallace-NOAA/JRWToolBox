

 rm.pts <- function(xy, Sequence = F, maxsize = 0.025, Add = F)
{
	" *** First point returned is a flag, remove if not needed!! ***"
	""
	items <- c("Continue by removing points one at a time", "Continue by removing a sequence on points", "Start over", 
		"All done (parent program will be informed of which points to remove)", "Quit and retain the original data", 
		"Quit and flag parent program to remove any stored points")
	x <- xy[, 1]
	y <- xy[, 2]
	id <- 1:nrow(xy)
	id.keep <- id
	Redraw <- T
	repeat {
		if(Redraw) {
			if(!Add)
				plot(x, y, type = "l")
			Plot.bubble(x, y, 1:length(x), maxsize = maxsize, num.col = 10, facets.num = 9)
		}
		bad <- identify(x, y, id[(id %in% id.keep)])
		if(length(bad) > 1 & Sequence)
			bad <- unique(min(bad):max(bad), bad)
		if(length(bad) > 0) {
			x <- x[ - bad]
			y <- y[ - bad]
			id.keep <- id.keep[ - bad]
			cat("\n\nAggregate point(s) selected:\n\n", sort(id[!(id %in% id.keep)]), "\n\n")
			if(!Add)
				plot(x, y, type = "l")
			Plot.bubble(x, y, 1:length(x), maxsize = maxsize, num.col = 10, facets.num = 9)
		}
		FLAG <- 1
		switch(menu(items) + 1,
			{
				x <- xy[, 1]
				y <- xy[, 2]
				cat("\nStarting over...\n")
				Redraw <- T
			}
			,
			{
				Sequence <- F
				cat("\nContinuing...\n")
				Redraw <- F
			}
			,
			{
				Sequence <- T
				cat("\nContinuing...\n")
				Redraw <- F
			}
			,
			{
				x <- xy[, 1]
				y <- xy[, 2]
				cat("\nStarting over...\n")
				Redraw <- T
			}
			,
			{
				break
			}
			,
			{
				x <- xy[, 1]
				y <- xy[, 2]
				id.keep <- id
				break
			}
			,
			{
				FLAG <- -1
				break
			}
			)
	}
	rm.vec <- c(FLAG, sort(id[!(id %in% id.keep)]))
	if(length(rm.vec[-1]) > 0)
		cat("\n\nAggregate point(s) selected:\n\n", rm.vec[-1], "\n\n")
	else cat("\n\nNo points selected:\n\n")
	invisible(rm.vec)
}


 renum <- function(x, no.num = F)
{
	if(no.num)
		dimnames(x)[[1]] <- rep("", nrow(x))
	else dimnames(x)[[1]] <- 1:nrow(x)
	x
}


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


 sortmat.f <- function(x, col = 1, reverse = F)
{
	if(reverse)
		x[rev(order(x[, col])),  ]
	else x[order(x[, col]),  ]
}


 Inside.Polygon <- function(pts, h = hull)
{
#
# Input:
#    pts -- (n x 2) matrix of test points
#      h -- ([k+1] x 2) matrix of (ordered) vertices, 
#                 [with last row = first row]
# Output:
#     (n x 1) logical vector indicating inclusion of tests points
#           in the convex polygon formed by h 
#
	n <- nrow(pts)
	k <- nrow(h) - 1
	if(!all(h[1,  ] == h[k + 1,  ])) {
		h <- rbind(h, h[1,  ])
		k <- k + 1
	}
#
# compute slopes and intercepts of boundary lines
#
	b <- diff(h[, 2])/diff(h[, 1])
	a <- diag(h[1:k,  ] %*% t(cbind( - b, 1)))	#
#   compute centroid and its residuals from boundary lines
#
	m <- rep(1/k, k) %*% h[1:k,  ]
	r0 <- m[2] - (a + b * m[1])	#
#  compute residuals of test points from boundary lines
#
	r <- outer(pts[, 2], rep(1, k)) - outer(pts[, 1], b) - outer(rep(1, n), a)
	signs <- (r %*% diag(r0)) > 0
	ans <- as.vector(signs %*% rep(1, k) == k)
	ans
}
