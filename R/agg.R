agg <- function(data, by, FUNS, na.rm = T)
{
#
#   DATE WRITTEN: 14 May 1998,  Updated: 06 Feb 2008
#   AUTHOR:  John R. Wallace (John.Wallace@noaa.gov)
#
   Min <- function(x)
       min(x, na.rm = na.rm)
         Max <- function(x)
       max(x, na.rm = na.rm)
     Mean <- function(x)
        mean(x, na.rm = na.rm)
     Var <- function(x)
       var(x, na.rm = na.rm)
     SD <- function(x)
      sqrt(var(x, na.rm = na.rm))
     SE <- function(x)
   {
      if(na.rm)
        sqrt(var(x, na.rm = na.rm)/sum(!is.na(x)))
      else
        sqrt(var(x)/length(x))      }
   Median <- function(x)
             median(x, na.rm = na.rm)
     N <- function(x)
          if(na.rm)
            sum(!is.na(x))
          else             length(x)
     HZ <- function(x)
   {
       x[!is.finite(x)] <- 0
       x[x == 0] <- min(x[x > 0])/2
       mean(log(x))
   }
     out <- aggregate.data.frame(data, by, eval(parse(text = FUNS[1])))
   if(length(FUNS) > 1) {
       for(i in 2:length(FUNS)) {
           tmp <- aggregate.data.frame(data, by, eval(parse(text = FUNS[i])))
           tmp <- tmp[, ncol(tmp)]
           out <- cbind(out, tmp)
       }
   }
   dimnames(out)[[2]][ - (1:length(by))] <- paste(names(data), FUNS, sep = ".")
   out
}


