ratioXoverY.SD.Boot <- function (x, y, N = 1e+06, XoverXplusY = FALSE, iter.min = 1e+05, 
    progress = 10000, plot. = T, tol = 0.01, Times.LT.tol = 5, 
    BurnIn.Prop = 0.3, delta = 1e-07, N.sub.loop = 100) 
{
    "  # Notes:  "
    "  # - XoverXplusY = TRUE gives the ratio and SD of sum(x)/sum(c(x, y)); if FALSE then sum(x)/sum(y) "
    "  # - This version has a sub-loop for generating sample boots (default of N.sub.loop = 100 works well). This gives substantially better performance.  "
    "  # - Tolerance [absolute relative error (ARE) * 1000] argument was adjusted to be approximately decimal accuracy in limited testing.  "
    "  # - Convergence criteria is number of bootstraps greater than iter.min and the number of the last 10 ARE less than tol, being greater than or equal to the Times.LT.tol value.  "
    "  # - Using multiple threads via the snow package did not give better performance.  "
    "  # - BurnIn.Prop argument is only for the amount of zoom of the middle graphic, it does not play into the calculations of SD.  "
	  "  # - N is the total number of iterations; progress is the number if iterations between updates.  "
    "  "
    "  # Usage  "
    "  "
    "  # x <- rlnorm(50, sdlog=5)  "
    "  # y <- rlnorm(50, sdlog=5)  "
    "  "
    "  # With tol = 0.01 elasped time is ~8 sec using MRO under Win7 with Intel Xeon 3.47 GHz CPU  "
    "  # system.time(tmp <- ratioXoverY.SD.Boot(x, y,  XoverXplusY = T, tol=0.01, plot=T, Times.LT.tol = 5)); cat(\"\n\"); tmp  "
    "  "
    "  # With tol = 0.001 tested elasped time was 51-218 sec (340 elasped sec on Tantulas; a Linux server with slower chips)  "
    "  # system.time(tmp <- ratioXoverY.SD.Boot(x, y,  XoverXplusY = T, tol=0.001, plot=T, Times.LT.tol = 5)); cat(\"\n\"); tmp  "
    "  "
    "  # Convergence is much slower for a simple ratio of sum(x)/sum(y)  "
    "  # system.time(tmp <- ratioXoverY.SD.Boot(x,y, tol=0.01, plot=T, Times.LT.tol = 5)); cat(\"\n\"); tmp  "
    "  "
    "  "
    "  "
    "  # ------------------ Functions ----------------------  "
    cumsd <- function(x) {
        N <- length(x)
        One.to.N.minus.1 <- c(1, 1:(N - 1))
        sqrt(cumsum(x^2)/One.to.N.minus.1 - (cumsum(x)^2)/((1:N) * 
            One.to.N.minus.1))
    }
    "  # ------------------ End Functions ----------------------  "
    "  "
    "  # ------------ Tolerance adjustment - found via testing ---------------------  "
    tol <- tol/1000
    "  # -------------------------------------------------------  "
    if (all(x %in% 0) & all(y %in% 0)) 
        return(NA)
    if (XoverXplusY) {
        " # First iteration  "
        X <- sample(x, rep = T)
        boot <- (mean(X) + delta)/(mean(c(X, sample(y, rep = T))) + delta)
        " # Second iteration  "
        X <- sample(x, rep = T)
        boot <- c(boot, (mean(X) + delta)/(mean(c(X, sample(y, rep = T))) + delta))
    }
    else {
        boot <- (mean(sample(x, rep = T)) + delta)/(mean(sample(y, 
            rep = T)) + delta)
        boot <- c(boot, (mean(sample(x, rep = T)) + delta)/(mean(sample(y, 
            rep = T)) + delta))
    }
    SD.OLD <- 0
    ARE.save <- 0
    for ( i in 1:(N/N.sub.loop)) {
        N.sub <- N.sub.loop - ifelse( i == 1, 2, 0)
        boot.tmp <- rep(NA, N.sub)
        for (j in 1:N.sub) {
            if (XoverXplusY) {
                X <- sample(x, rep = T)
                boot.tmp[j] <- (mean(X) + delta)/(mean(c(X, sample(y, rep = T))) + delta)
            }
            else {
                boot.tmp[j] <- (mean(sample(x, rep = T)) + delta)/(mean(sample(y, 
                  rep = T)) + delta)
            }
        }
		boot <- c(boot, boot.tmp)
		ARE <- abs(sd(boot) - SD.OLD)/(sd(boot))
        ARE.save <- c(ARE.save, ARE)
		SD.OLD <- sd(boot)
		if (progress > 0) { 
            if (i%%(progress/N.sub.loop) == 0 | (sum(tail(ARE.save, 10) < tol, na.rm = TRUE) >= Times.LT.tol & length(boot) > iter.min)) {
                SD <- sd(boot)
                cat("\nTotal Bootstraps = ", length(boot), "; SD = ", 
                  SD, "; ARE = ", ARE, "; Num of times lt tol. = ", 
                  sum(tail(ARE.save, 10) < tol, na.rm = TRUE), 
                  "               ")
				  if (plot.) {
                  par(mfrow = c(3, 1))
                  N.BurnIn <- round(BurnIn.Prop * length(boot))
                  plot(cumsd(boot), type = "l", ylim = c(0.975 * 
                    SD, 1.025 * SD), ylab = "Bootstrapped SD")
                  abline(v = iter.min)
                  abline(v = N.BurnIn, col = "red")
                  abline(h = SD, col = "green")
                  plot((N.BurnIn):length(boot), cumsd(boot)[(N.BurnIn):length(boot)], 
                    type = "l", ylab = "Bootstrapped SD")
                  abline(h = SD, col = "green")
                  plot(ARE.save[-(1:10)], ylab = 'ARE', type = "p", pch = ".")
                  abline(h = tol, col = "blue")
                }
            }
        }
        if (sum(tail(ARE.save, 10) < tol, na.rm = TRUE) >= Times.LT.tol & length(boot) > iter.min) {
			 break
        }
   	    if (i == N) 
            Tol <- paste("No convergence in", N, "attempts")
        else Tol = abs(sd(boot) - SD.OLD)/sd(boot)
        flush.console()
	}
	cat("\n\n")
    if (plot.) {
        par(mfrow = c(3, 1))
        N.BurnIn <- round(BurnIn.Prop * length(boot))
        plot(cumsd(boot), type = "l", ylim = c(0.975 * sd(boot), 
            1.025 * sd(boot)), ylab = "Bootstrapped SD")
        abline(v = iter.min)
        abline(v = N.BurnIn, col = "red")
        abline(h = SD, col = "green")
        plot((N.BurnIn):length(boot), cumsd(boot)[(N.BurnIn):length(boot)], 
            type = "l", ylab = "Bootstrapped SD", xlab = paste0("Last ", 100*(BurnIn.Prop), "% of the Bootstraps"))
        abline(h = SD, col = "green")
        hist(boot)
    }
    if (XoverXplusY) {
        Ratio <- sum(x)/sum(c(x, y))
    }
    else {
        Ratio <- sum(x)/sum(y)
    }
    data.frame(Ratio = Ratio, SD = sd(boot), Total.Iter = length(boot), 
        Tol = Tol, LT.tol = sum(tail(ARE.save, 10) < tol, na.rm = TRUE))
}
