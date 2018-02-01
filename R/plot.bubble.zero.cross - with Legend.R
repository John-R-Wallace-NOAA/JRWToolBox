plot.bubble.zero.cross <- function (xyzOrg, group = rep("A", nrow(xyz)), maxsize = scale.size * diff(range(xyzOrg[, 2])), scale.size = 0.07, add = F, xlab = dimnames(xyz)[[2]][1], 
    ylab = dimnames(xyz)[[2]][2], range.bump = F, cross.cex = 1, adj = NULL, fill.col = c("green", "red", "blue", "cyan", "black"), fill.col.alpha = 0.2, border.col = "black", 
    cross.col = { if (is.null(fill.col)) border.col else fill.col }, cross.col.alpha = ifelse(fill.col.alpha + 0.65 > 1, 1, fill.col.alpha + 0.5), border.lwd = 1.25, 
    Grid.circle = F, legend = TRUE, legLoc = c(0.1, 0.25), legCol = 'grey4', legAlpha = 0.5, legUnits = 'Metric Tons', legNsmall = 1, ...) 
{
' **** Data is proportional to the area of the circle **** '
    "%r1%" <- function(e1, e2) ifelse(e1%%e2 == 0, e2, e1%%e2)
    col.alpha <- function(col, alpha = 0.5) {
        FUNC <- function(col, alpha = alpha) {
            COL <- col2rgb(col)/255
            rgb(red = COL[1], green = COL[2], blue = COL[3], 
                alpha = alpha)
        }
        for (i in 1:length(col)) col[i] <- FUNC(col[i], alpha = alpha[i %r1% 
            length(alpha)])
        col
    }
    if (Grid.circle) 
        require(grid)
    if (!is.null(fill.col)) 
        fill.col <- col.alpha(fill.col, fill.col.alpha)
    if (!is.null(cross.col) & !all(cross.col == border.col)) 
        cross.col <- col.alpha(cross.col, cross.col.alpha)
    xyzSqrt <- xyzOrg
    xyzSqrt[, 3] <- sign(xyzSqrt[, 3]) * sqrt(abs(xyzSqrt[, 3]))
	xyz <- xyzSqrt
    xyz[, 3] <- (maxsize * xyzSqrt[, 3])/max(xyzSqrt[, 3], na.rm = T)
    tf <- !apply(xyz, 1, function(x) any(is.na(x)))
    xyz <- xyz[tf, ]
    group <- group[tf]
    Groups <- unique(group)
    N <- length(Groups)
	if (!add) {
        if (range.bump) {
            xlim <- c(min(xyz[, 1], na.rm = T) - 0.2 * diff(range(xyz[, 
                1], na.rm = T)), max(xyz[, 1], na.rm = T) + 0.2 * 
                diff(range(xyz[, 1], na.rm = T)))
            ylim <- c(min(xyz[, 2], na.rm = T) - 0.2 * diff(range(xyz[, 
                2], na.rm = T)), max(xyz[, 2], na.rm = T) + 0.2 * 
                diff(range(xyz[, 2], na.rm = T)))
            plot(xyz[, 1], xyz[, 2], type = "n", xlim = xlim, 
                ylim = ylim, xlab = xlab, ylab = ylab)
        }
        else plot(xyz[, 1], xyz[, 2], type = "n", xlab = xlab, 
            ylab = ylab)
    }
    for (j in 1:N) {
        XYZ <- xyz[group %in% Groups[j], ]
        for (i in 1:nrow(XYZ)) {
            if (XYZ[i, 3] == 0) 
                points(XYZ[i, 1], XYZ[i, 2], pch = 3, cex = cross.cex, 
                  col = cross.col[j %r1% length(cross.col)], 
                  lwd = 1)
            else {
                if (Grid.circle) 
                  grid.circle(XYZ[i, 1], XYZ[i, 2], XYZ[i, 3], 
                    default.units = "native")
                else circle.f(XYZ[i, 1], XYZ[i, 2], XYZ[i, 3], 
                  adj = adj, fill.col = fill.col[j %r1% length(fill.col)], 
                  lwd = border.lwd[j %r1% length(border.lwd)], 
                  border.col = border.col[j %r1% length(border.col)], 
                  ...)
            }
        }
    }
	# assign('xyz', xyz, pos=1)
	# assign('xyzOrg', xyzOrg, pos=1)
    if(legend) {
	   
	   	pretVec <-  pretty(c(0, max(xyzOrg[,3], na.rm=TRUE)))
		Usr <- par()$usr
		
		Np <- length(pretVec)
		
		Large <- pretVec[Np -1]
		Mid <- pretVec[Np/1.5]
		Small <- pretVec[Np/2.5]
		
		# printf(max(xyzOrg[,3], na.rm=TRUE))
		# printf(pretVec)
		
        text(Usr[1] + (legLoc[1] + 0.05) * (Usr[2] - Usr[1]), Usr[3] + legLoc[2] * (Usr[4] - Usr[3]), legUnits, cex=0.9) 		
		text(Usr[1] + legLoc[1] * (Usr[2] - Usr[1]), Usr[3] + (legLoc[2] - 0.03) * (Usr[4] - Usr[3]), "+")
		text(Usr[1] + (legLoc[1] + 0.125) * (Usr[2] - Usr[1]), Usr[3] + (legLoc[2] - 0.03) * (Usr[4] - Usr[3]), "  0.0", adj = 1, cex=0.9)
		
		circle.f(Usr[1] + legLoc[1] * (Usr[2] - Usr[1]), Usr[3] + (legLoc[2] - 0.075) * (Usr[4] - Usr[3]), maxsize * sqrt(Small)/max(xyzSqrt[, 3], na.rm = T), fill.col=col.alpha(legCol, legAlpha))
		text(Usr[1] + (legLoc[1] + 0.125) * (Usr[2] - Usr[1]), Usr[3] + (legLoc[2] - 0.075) * (Usr[4] - Usr[3]), format(Small, nsmall=legNsmall), adj = 1, cex=0.9)
						
		circle.f(Usr[1] + legLoc[1] * (Usr[2] - Usr[1]), Usr[3] + (legLoc[2] - 0.115) * (Usr[4] - Usr[3]), maxsize * sqrt(Mid)/max(xyzSqrt[, 3], na.rm = T), fill.col=col.alpha(legCol, legAlpha))
		text(Usr[1] + (legLoc[1] + 0.125) * (Usr[2] - Usr[1]), Usr[3] + (legLoc[2] - 0.115) * (Usr[4] - Usr[3]), format(Mid, nsmall=legNsmall), adj = 1, cex=0.9)
		
		circle.f(Usr[1] + legLoc[1] * (Usr[2] - Usr[1]), Usr[3] + (legLoc[2] - 0.175) * (Usr[4] - Usr[3]), maxsize * sqrt(Large)/max(xyzSqrt[, 3], na.rm = T), fill.col=col.alpha(legCol, legAlpha))
		text(Usr[1] + (legLoc[1] + 0.125) * (Usr[2] - Usr[1]), Usr[3] + (legLoc[2] - 0.175) * (Usr[4] - Usr[3]), format(Large, nsmall=legNsmall),  adj = 1, cex=0.9)

    }
    invisible()
}
