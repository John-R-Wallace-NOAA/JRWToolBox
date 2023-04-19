
plot.loess <- function (x, y, span = 0.75, type = "p", jitter.x = F, factor = 1, line.col = "green", 
           xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), ...) {
           
   if(jitter.x)
      plot(jitter(x, factor), y, type = type, xlab = xlab, ylab = ylab, ...)
   else
      plot(x, y, type = type, xlab = xlab, ylab = ylab, ...)

   loess.line(x, y, span = span, col = line.col)
}


plot.loess.range <- function (x, y, span = c(1/6, 1/3, 1/2, 2/3, 3/4, 5/6, 1, 2, 3), type = "p", jitter.x = F, factor = 1, 
                      line.col = rainbow(length(span)), xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), ...) {

   if(jitter.x)
      plot(jitter(x, factor), y, type = type, xlab = xlab, ylab = ylab, ...)
   else
      plot(x, y, type = type, xlab = xlab, ylab = ylab, ...)

   for (i in 1:length(span))
      loess.line(x, y, span = span[i], col = line.col[i])
}

