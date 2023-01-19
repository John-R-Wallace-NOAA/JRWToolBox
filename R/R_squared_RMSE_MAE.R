
R_squared_RMSE_MAE <- function(Pred, Obs, na.rm = TRUE) {

    c(R_squared = signif(cor(Pred, Obs)^2, 6), RMSE = signif(sqrt(mean((Pred - Obs)^2, na.rm = TRUE)), 6), 
         MAE = signif(mean(abs(Pred - Obs), na.rm = TRUE), 6))
}       
 
