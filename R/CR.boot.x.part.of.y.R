function(x, y, N=1e6, iter.min = 100000, progress = 10000, plot. = T, tol = 0.00001, Times.LT.tol = 200, BurnIn.Prop =  0.30, delta = 0.0000001) {

 # ------------------ Functions ----------------------
  cumsd <- function(x) {

    N <- length(x)
    One.to.N.minus.1 <- c(1,1:(N-1))
    sqrt(cumsum(x^2)/One.to.N.minus.1 - (cumsum(x)^2)/((1:N)*One.to.N.minus.1))
  }
 # ---------------------------------------------------

   if(all(x %in% 0) & all(y %in% 0))
       return(NA)

  X <- sample(x, rep=T)
  boot <- (sum(X) + delta)/(sum(c(X, sample(y, rep=T))) + delta)

  X <- sample(x, rep=T)
  boot <- c(boot, (sum(X) + delta)/(sum(c(X, sample(y, rep=T))) + delta))

  SD.OLD <- 0
  LT.tol <- 0
  ARE.save <- 0

  for (i in 3:N) {

      ARE <- abs(sd(boot) - SD.OLD)/(sd(boot))
      ARE.save <- c(ARE.save, ARE)

      if(progress > 0) {
          if(i %% progress == 0) {
              cat("\rIter = ", i, "; SD = ", sd(boot),  "; ARE = ", ARE, "; Num of times lt tol. = ", LT.tol, "               "); flush.console() 
              if(plot.) {
                 par(mfrow=c(3,1))
                 plot(cumsd(boot), type='l', ylim=c(.975*sd(boot), 1.025*sd(boot))); abline(v = iter.min)
                 plot(cumsd(boot)[-(1:(round(BurnIn.Prop * length(boot))))], type='l')
                 # plot(cumsd(boot)[(length(boot) - 9):length(boot)], type='l')
                 # plot(ARE.save[-(1:(round(BurnIn.Prop * length(boot))))], type='p', pch='.'); abline(h=tol)
                 plot(ARE.save[-(1:1000)], type='p', pch='.'); abline(h=tol)
              }
          }
      }

      if (ARE < tol & !is.na(ARE) & i > iter.min) {
             
          LT.tol <- LT.tol + 1
          if(LT.tol == Times.LT.tol)
               break

      } else 
           LT.tol <- 0
       

      SD.OLD <- sd(boot)

      X <- sample(x, rep=T)
      boot <- c(boot, (sum(X) + delta)/(sum(c(X, sample(y, rep=T))) + delta))
  }

  if( i == N)
    Tol <-  paste("No convergence in", N, "attempts")
  else
    Tol = abs(sd(boot) - SD.OLD)/sd(boot)


  if(progress > 0) 
    cat("\n\n"); flush.console() 

  if(plot.) {
    par(mfrow=c(4,1))
    plot(cumsd(boot), type='l', ylim=c(.975*sd(boot), 1.025*sd(boot)))
    plot(cumsd(boot)[-(1:(round(BurnIn.Prop * length(boot))))], type='l')
    plot(cumsd(boot)[(length(boot) - 9):length(boot)], type='l')
    hist(boot)
  }

  data.frame(CR = sum(x)/sum(c(x, y)), SD = sd(boot), Iter = i, Tol = Tol, LT.tol = LT.tol)
}
