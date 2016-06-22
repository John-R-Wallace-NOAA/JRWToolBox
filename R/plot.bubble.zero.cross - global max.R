
plot.bubble.zero.cross  <- 
function(xyz, maxsize = scale.size * diff(range(xyz[, 2])), scale.size = 0.07, add = F, xlab = dimnames(xyz)[[2]][1], ylab = dimnames(xyz)[[2]][2], range.bump = F, cross.cex = 1, 
	adj=NULL, fill.col = NULL, border.col = 1, cross.col = border.col, border.lwd = 2, prop.to.area = T, Grid.circle = F, globalMax = NULL, ...)
{

#
#   DATE WRITTEN:  Circa 2000      LAST REVISED:   19 Jan 2005
#   AUTHOR:  John R. Wallace (John.Wallace@noaa.gov)
	xyz <- xyz[rev(order(xyz[, 3])),  ]
	""
	if(prop.to.area)
		xyz[, 3] <- sign(xyz[, 3]) * sqrt(abs(xyz[, 3]))
	""
	if(!add) {
		if(range.bump) {
			xlim <- c(min(xyz[, 1], na.rm = T) - 0.2 * diff(range(xyz[, 1], na.rm = T)), max(xyz[, 1], na.rm = T) + 0.2 * diff(range(xyz[, 1], 
				na.rm = T)))
			ylim <- c(min(xyz[, 2], na.rm = T) - 0.2 * diff(range(xyz[, 2], na.rm = T)), max(xyz[, 2], na.rm = T) + 0.2 * diff(range(xyz[, 2], 
				na.rm = T)))
			plot(xyz[, 1], xyz[, 2], type = "n", xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab)
		}
		else plot(xyz[, 1], xyz[, 2], type = "n", xlab = xlab, ylab = ylab)
	}
	""
	
        if(is.null(globalMax))
	    xyz[, 3] <- (maxsize * xyz[, 3])/max(xyz[, 3], na.rm = T)
        else
            xyz[, 3] <- (maxsize * xyz[, 3])/globalMax
	nCol <- length(fill.col)
	xyz <- na.omit(xyz)
	xyz <- sort.f(xyz, 3)
	xyz <- xyz[order(xyz[, 3]),  ]
	""
	for(i in 1:nrow(xyz)) {
		if(xyz[i, 3] == 0)
			points(xyz[i, 1], xyz[i, 2], pch = 3, cex = cross.cex, col = cross.col, lwd = 0)
		else {
		
		if(Grid.circle)
                  grid.circle(xyz[i, 1], xyz[i, 2], xyz[i, 3], default.units="native")
                else
		  circle.f(xyz[i, 1], xyz[i, 2], xyz[i, 3], adj=adj, fill.col = fill.col[i %% nCol + 1], lwd = border.lwd, border.col = border.col, ...)
		  
		}
	}
	invisible()
}

