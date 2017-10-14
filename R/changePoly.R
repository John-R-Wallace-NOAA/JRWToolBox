
changePoly <- function(xy, col = "blue", alpha = 0.25, closePoly = TRUE, lty = 1, colBg = par()$bg, ...) {
   movePtPoly <- function(xy) {
      while (length(PtRow <- identify(xy, labels = "", n = 1)) == 1) {
         newPt <- locator(1)
         lines(xy$x, xy$y, lty = lty, col = ifelse(colBg == "transparent", "white", colBg), type = "o", ...)
         xy[PtRow, ] <- newPt
         lines(xy$x, xy$y, lty = lty, col = "black", type = "o", ...)
      }
	  xy
   }
' # ------------------------- '		
    addPtPoly <- function(xy) {
      while (T) {
	      N <- nrow(xy)  
	      newPt <- locator(1)
		  if(is.null(newPt)) break
          lines(xy$x, xy$y, lty = lty, col = ifelse(colBg == "transparent", "white", colBg), type = "o", ...)
          distOrd <- order(apply(xy, 1, function(vw) gdist(newPt$x, newPt$y, vw[1], vw[2])))[1:2]
          if(distOrd[1] == 1) 
             Out <- rbind(newPt, xy[1:(N-1),], newPt)
          if(distOrd[1] == N) 
             Out <- rbind(newPt, xy[2:N,], newPt)
          if(distOrd[1] < distOrd[2])
             Out <- rbind(xy[1:distOrd[1], ], newPt, xy[(distOrd[1] + 1):N, ])
          if(distOrd[1] > distOrd[2])
             Out <- rbind(xy[1:(distOrd[1] - 1), ], newPt, xy[distOrd[1]:N, ])
  
          xy <- Out 
          lines(xy$x, xy$y, lty = lty, col = "black", type = "o", ...)
	  }
	  xy
    }   
 ' # ------------------------- '
    polySize <- function(xy) {
	   JRWToolBox::lib(polyclip, require = FALSE)
	   JRWToolBox::lib(sp, require = FALSE)
	   while (T) {
		 newPt <- locator(1)
		 if(is.null(newPt)) break
         lines(xy$x, xy$y, lty = lty, col = ifelse(colBg == "transparent", "white", colBg), type = "o", ...)
		 # lines(xy$x, xy$y, lty = lty, col = col.alpha('cyan', 0.25), ...)
	     minDist <- min(apply(xy, 1, function(vw) gdist(newPt$x, newPt$y, vw[1], vw[2], units = 'nm')/60))
		 if(as.logical(sp::point.in.polygon(newPt$x, newPt$y, xy$x, xy$y)))
		     minDist <- - minDist
         xy <- data.frame(polyclip::polyoffset(list(x=xy$x[-1], y=xy$y[-1]), minDist)[[1]])
		 lines(xy$x, xy$y, lty = lty, col = "black", type = "o", ...)
      }
	  xy
   }
' # ------------------------- Main ---------------------------------------------------------------------------------- ' 
  lines(xy$x, xy$y, col = "black", lty = lty, type = "o", ...)
   while(TRUE) 
       xy <- switch(menu(c("Move points", "Add points", "Resize the polygon", "Quit")), movePtPoly(xy), addPtPoly(xy), polySize(xy), break)
   lines(xy$x, xy$y, lty = lty, col = ifelse(colBg == "transparent", "white", colBg), type = "o", ...)
   if (closePoly) {
        xy$x[length(xy$x)] <- xy$x[1]
        xy$y[length(xy$y)] <- xy$y[1]
    }
    polygon(xy$x, xy$y, col = col.alpha(col, alpha), lty = lty, ...)
    invisible(xy)
}
