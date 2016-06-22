
if(F) {

  load('plot.conf.band.data.RData')

   plot(x,y, type='n')
   plot.conf.bands(list(x=x,y=y), lower, upper, type='l', lwd=2)
   plot.conf.bands(line2, lower2, upper2, type='l', lwd=2)

   plot(x,y, type='n')
   plot.conf.bands(list(x=x,y=y), lower, upper, col.area = 'red', type='l', lwd=2)
   plot.conf.bands(line2, lower2, upper2,  col.area = 'green', type='l', lwd=2)

   plot(x,y, type='n')
   plot.conf.bands(list(x=x,y=y), lower, upper, type='l', lwd=2)
   plot.conf.bands(line2, lower2, upper2, type='l', col.area = 'grey40', lwd=2, alpha=0.5)


}


plot.conf.bands <- function(Line, conf.lower, conf.upper, col.line = 'black', col.area = 'grey', alpha = 0.5, ...) {
  
  col.alpha <- function (col, alpha = 0.5) 
    {
      COL <- col2rgb(col)/255
      rgb(red = COL[1], green = COL[2], blue = COL[3], alpha = alpha)
    }


  if(is.list(conf.lower))
      conf.lower <- cbind(conf.lower$x, conf.lower$y)

  if(is.list(conf.upper))
      conf.upper <- cbind(conf.upper$x, conf.upper$y)


  lines(Line, col = col.line, ...)
              
  polygon(c(conf.lower[, 1], rev(conf.upper[, 1])), c(
                conf.lower[, 2], rev(conf.upper[, 2])), col = col.alpha(col.area, alpha = alpha))
}


