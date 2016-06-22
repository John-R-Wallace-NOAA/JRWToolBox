plot.lowess.lsfit <- 
function (x, y, smoothing.param = 2/3, type = "p", jitter.x = F, factor = 1, line.col = c("green", "blue"), glmSummary = TRUE, ...) {
   
   if(jitter.x)
      plot(jitter(x, factor), y, type = type, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), ...)
   else
      plot(x, y, type = type, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), ...)

   lowess.line(x, y, smoothing.param = smoothing.param, col = line.col[1])

   abline(lsfit(x, y), col = line.col[2])
 
   if(glmSummary)
       summary(glm(as.formula(paste(deparse(substitute(y)), " ~ ", deparse(substitute(x))))))
   else
     ls.print(lsfit(x, y))

}
