Correlation_R_squared_RMSE_MAE_SAD <- 
function(Obs, Pred, na.rm = TRUE) {
    signif(data.frame(
      Correlation = cor(Pred, Obs),
        R_squared = cor(Pred, Obs)^2, 
             RMSE = sqrt(mean((Pred - Obs)^2, na.rm = na.rm)), 
              MAE = mean(abs(Pred - Obs), na.rm = na.rm),
              SAD = sum(abs(Pred - Obs), na.rm = na.rm),
              N = ifelse(na.rm, sum(!is.na(Obs)), length(Obs))
    ), 6)
}
