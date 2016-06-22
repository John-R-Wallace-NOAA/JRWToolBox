factor.f <- function(x, by = NULL, breaks, labels = levels(tmp), include.lowest = F, verbose=F)
{

  # ************ Note: the breaks need to be in acesending order. ************

  # Examples:
  # factor.f(income, breaks = c(0,30000,70000,200000), labels=c("low","mid","high"))
  # factor.f(income, breaks = c(0,30000,70000,200000))
  # 

  if(!is.null(by)) {

        FLOOR <- floor.f(min(x, na.rm=T), by)
        CEILING <- ceiling.f(max(x, na.rm=T), by)

        breaks <- seq(FLOOR, CEILING, by=by)
        
        MID <- seq(FLOOR + by/2, CEILING, by=by)
        x <- c(MID, x)
  }

 # Add midpoint of breaks to x so that labels always has something to work with

  tmp <- cut(c(diff(breaks), x), breaks = breaks, include.lowest = include.lowest)


  if(verbose) {
        cat("\n")
        printf(breaks)
        printf(levels(tmp))
        cat("\n")
   }

  out <- factor(tmp, labels = labels)

 # Take out extra midpoints

  out <- out[-(1:length(diff(breaks)))]

  if(verbose) {
        catf("\n")
        printf(aggregate(list(x=x), list(x.factor=out), len))
        catf("\n")
        plot(x ~ out, xlab="Factor", ylab="Input")
  }

if(!is.null(by))

  out[-(1:length(MID))]

else out

}
