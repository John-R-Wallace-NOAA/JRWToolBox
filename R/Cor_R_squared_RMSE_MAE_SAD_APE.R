
Cor_R_squared_RMSE_MAE_SAD_APE <- 
function(Obs, Pred, na.rm = TRUE, digits = 6) {
  
	na <- function(x, na.rm = FALSE) { if(na.rm) x[!is.na(x)] else x }
	
                 ObsZ <- Obs
	            PredZ <- Pred
      ObsZ[ObsZ == 0] <- min(ObsZ[ObsZ != 0])/2
    PredZ[PredZ == 0] <- min(PredZ[PredZ != 0])/2
	
    print(out <- signif(data.frame(
      Correlation = cor(Pred, Obs),
        R_squared = cor(Pred, Obs)^2, 
             RMSE = sqrt(mean((Pred - Obs)^2, na.rm = na.rm)), 
              MAE = mean(abs(Pred - Obs), na.rm = na.rm),
              SAD = sum(abs(Pred - Obs), na.rm = na.rm),
			  APE = mean(apply(cbind(ObsZ, PredZ), 1, function(x) 100 * sum(abs(na(x, na.rm = na.rm) - mean(x, na.rm = na.rm))/mean(x, na.rm = na.rm))/length(na(x, na.rm = na.rm))), na.rm = na.rm),
              N = ifelse(na.rm, nrow(na.omit(cbind(Obs, Pred))), length(Obs))
    ), digits = digits))
	cat("\n", '   Note: The FSA package appears to add a very small number to the zero values for APE; here the zeros are set to half of the minimum non-zero value.\n\n')
	invisible(out)
}

