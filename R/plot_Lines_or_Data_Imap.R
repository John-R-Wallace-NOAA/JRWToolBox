
plot_Lines_or_Data_Imap <- function(XY, type = c('l', 'p', 'o', 'b')[1], labels = FALSE, colLine = 'green', colPts = NULL, zoom = FALSE, figureDelta = 0.2, labelDelta = 0.2, cex = 0.75) {

   imap(longlat = list(world.h.land, world.h.borders), col = c("black", "cyan"), poly = c("grey40", NA), zoom = zoom, 
        longrange = c(min(XY[,1]) - figureDelta, max(XY[,1]) + figureDelta),
         latrange = c(min(XY[,2]) - figureDelta, max(XY[,2]) + figureDelta))
   
   N <- nrow(XY)
   lines(XY, type = type, col = colLine)  
   if(!is.null(colPts))
       points(XY, col = colPts)   
   if(is.logical(labels)) {
       if(labels) 
            text(XY + data.frame(rep(labelDelta, N), rep(0, N)), label =  seq_along(XY[,1]), cex = cex)
   } else 
       text(XY + data.frame(rep(labelDelta, N), rep(0, N)), label = labels, cex = cex)
}
