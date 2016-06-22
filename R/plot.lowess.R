plot.lowess <- 
function (x, y, smoothing.param = 2/3, type = "p", jitter.x = F, factor = 1, line.col = "green", 
           xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), ...) 
{
   if(jitter.x)
      plot(jitter(x, factor), y, type = type, xlab = xlab, ylab = ylab, ...)
   else
      plot(x, y, type = type, xlab = xlab, ylab = ylab, ...)

   lowess.line(x, y, smoothing.param = smoothing.param, col = line.col)
}


plot.lowess.range <- 
function (x, y, smoothing.param = c(1/6, 1/3, 1/2, 2/3, 5/6), type = "p", jitter.x = F, factor = 1, line.col = "green", 
    ...) 

{
   if(jitter.x)
      plot(jitter(x, factor), y, type = type, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), ...)
   else
      plot(x, y, type = type, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), ...)

   for (i in 1:length(smoothing.param))
      lowess.line(x, y, smoothing.param = smoothing.param[i], col = rainbow(length(smoothing.param))[i])
  
}



