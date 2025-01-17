 Cor_R_squared_RMSE_MAE_SAD_APE <- function(Obs, Pred, na.rm = TRUE, digits = 6) {
   
    if(na.rm) {
       no_na <- na.omit(data.frame(Obs, Pred))
       Obs <- no_na$Obs
       Pred <- no_na$Pred
    }
  
    APE_j <- function(obsPredPair) {
              meanPair <- mean(obsPredPair)
                  if(meanPair == 0) {
                           meanVec <- apply(cbind(Obs, Pred), 1, mean)
                       meanPair_Den <- min(meanVec[meanVec > 0])/2
                  } else
                       meanPair_Den <- meanPair
        100 * sum(abs(obsPredPair - meanPair)/meanPair_Den)/length(obsPredPair)
    }
    
    signif(data.frame(
      Correlation = cor(Pred, Obs),
        R_squared = cor(Pred, Obs)^2, 
             RMSE = sqrt(mean((Pred - Obs)^2)), 
              MAE = mean(abs(Pred - Obs)),
              SAD = sum(abs(Pred - Obs)),
              APE = mean(apply(cbind(Obs, Pred), 1, APE_j)),
                N = length(Obs)
    ), digits = digits)
}

# Cor_R_squared_RMSE_MAE_SAD_APE <- function(Obs, Pred, na.rm = TRUE, digits = 6) {
#    
#     APE_j <- function(obsPredPair, na.rm = TRUE) {
#               na <- function(x, na.rm = FALSE) { if(na.rm) x[!is.na(x)] else x }
#               meanPair <- mean(obsPredPair, na.rm = na.rm)
#                   if(meanPair == 0) {
#                            meanVec <- apply(na.omit(cbind(Obs, Pred)), 1, mean)
#                        meanPair_Den <- min(meanVec[meanVec > 0])/2
#                   } else
#                        meanPair_Den <- meanPair
#         100 * sum(abs(na(obsPredPair, na.rm = na.rm) - meanPair)/meanPair_Den)/length(na(obsPredPair, na.rm = na.rm))
#     }
#     
#     signif(data.frame(
#       Correlation = cor(Pred, Obs, use = ifelse(na.rm, "pairwise.complete.obs", "everything")),
#         R_squared = cor(Pred, Obs, use = ifelse(na.rm, "pairwise.complete.obs", "everything"))^2, 
#              RMSE = sqrt(mean((Pred - Obs)^2, na.rm = na.rm)), 
#               MAE = mean(abs(Pred - Obs), na.rm = na.rm),
#               SAD = sum(abs(Pred - Obs), na.rm = na.rm),
#               APE = mean(apply(cbind(Obs, Pred), 1, APE_j, na.rm = na.rm), na.rm = na.rm),
#                 N = ifelse(na.rm, nrow(na.omit(cbind(Obs, Pred))), length(Obs))
#     ), digits = digits)
# }
