
Cor_R_squared_RMSE_MAE_SAD_APE <- function(Obs, Pred, na.rm = TRUE, digits = 6) {
   
    APE_j <- function(obsPredPair, na.rm = TRUE) {
	      na <- function(x, na.rm = FALSE) { if(na.rm) x[!is.na(x)] else x }
	      meanPair <- mean(obsPredPair, na.rm = na.rm)
		  if(meanPair == 0) {
			   meanVec <- apply(na.omit(cbind(Obs, Pred)), 1, mean)
		       meanPair_Den <- min(meanVec[meanVec > 0])/2
		  } else
		       meanPair_Den <- meanPair
        100 * sum(abs(na(obsPredPair, na.rm = na.rm) - meanPair)/meanPair_Den)/length(na(obsPredPair, na.rm = na.rm))
    }
    
    print(out <- signif(data.frame(
      Correlation = cor(Pred, Obs),
        R_squared = cor(Pred, Obs)^2, 
             RMSE = sqrt(mean((Pred - Obs)^2, na.rm = na.rm)), 
              MAE = mean(abs(Pred - Obs), na.rm = na.rm),
              SAD = sum(abs(Pred - Obs), na.rm = na.rm),
              APE = mean(apply(cbind(Obs, Pred), 1, APE_j, na.rm = na.rm), na.rm = na.rm),
                N = ifelse(na.rm, nrow(na.omit(cbind(Obs, Pred))), length(Obs))
    ), digits = digits))
    invisible(out)
}
