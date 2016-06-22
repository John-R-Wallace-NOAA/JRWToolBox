function(pts = NULL, cex = 0.5, levels = c(30, 100, 300, 700), US.EEZ = T, zoom = F, longrange = c(-132, -117), latrange = c(30, 
	51), shade = F, new.graph = T, identify. = F, pts.col = 1, Size.Graph = 1, ...)
{
	plot.bubble.zero.cross <- function(x, y, z, z.max = NULL, maxsize = 0.3, cross.cex = 0.5, cross.col = 2, num.col = 4, 
		colors = c("magenta", "red4", "green4", "blue2", "green2", "blue3", "olivedrab", "seagreen2", "pink", 
                "orange")[1:num.col], prop.to.area = T, lwd = 0)
	{
		xx <- cbind(x, y, z)[order(abs(z)),  ]
		N <- nrow(xx)
		if(prop.to.area)
			xx[, 3] <- sqrt(xx[, 3])
		if(is.null(z.max))
			z.max <- ifelse(max(xx[, 3]) == 0, 1, max(xx[, 3]))
		xx[, 3] <- (maxsize * xx[, 3])/z.max
		colnums <- colors[cut(xx[, 3], num.col)]
		for(i in 1:N) {
			if(xx[i, 3] == 0)
				points(xx[i, 1], xx[i, 2], pch = 3, cex = cross.cex, col = cross.col, lwd = 0)
			else circle.f(xx[i, 1], xx[i, 2], xx[i, 3], col = colnums[i], lwd = lwd)
		}
		invisible()
	}
	""
	""
	bathy.agg<-function(level) {

           out<-NULL
           for(i in 1:length(nw.cont)) {
	     if(nw.cont[[i]]$level == level)
                out<-rbind(out, cbind(x=nw.cont[[i]]$x, y= - nw.cont[[i]]$y), c(NA,NA))
           }
           out
       }
""
""
	
	"Bathemetry is in fathoms"
	""
	"graphics.off()"
	""
	coast.bath <- list(wc.world = coast.line.100k.pt)
	Add.ons <- 1
	""
	
	nw.cont <- contourLines(unique(PacCoast.Grid[, 1]),  - unique(PacCoast.Grid[, 2]), matrix(PacCoast.Grid[, 3], nrow = 1216), 
		levels =  - levels)
	
	""
	if(US.EEZ) {
		coast.bath[[2]] <- US.EEZ.Line
		Add.ons <- Add.ons + 1
	}
	for(i in 1:length(levels))
		coast.bath[[i + Add.ons]] <- bathy.agg(- levels[i])
	""
	if(new.graph)
		windows(8.5 * Size.Graph, 11 * Size.Graph)
	""
	if((is.matrix(pts) | is.data.frame(pts)) & ifelse(is.null(ncol(pts)), NA, ncol(pts)) == 3) {
		out <- imap(coast.bath, longrange = longrange, latrange = latrange, line.col = c("black", "black", "blue", "red",
                             "green", "cyan", "aquamarine"), 
			zoom = zoom, cex = cex)
		plot.bubble.zero.cross(pts[, 1], pts[, 2], pts[, 3], ...)
	}
	else {
		if(is.data.frame(pts))
			pts <- list(pts)
		""
		N <- length(pts)
		if(shade)
			out <- imap.pts(coast.bath, pts = pts, longrange = longrange, latrange = latrange, line.col = c(1, 1, 17:
				100), zoom = zoom, ident = identify., cex = cex, pts.col = rep(pts.col, len = N), ...)
		else out <- imap.pts(coast.bath, pts = pts, longrange = longrange, latrange = latrange, line.col = c(1, 1, 4, 2, 
				3, 6, 7:16), zoom = zoom, ident = identify., cex = cex, pts.col = rep(pts.col, len = N), ...)
	}
	invisible(out)
}
