extractAIC.glm.Burnham.Anderson <- function(fit, scale = 0, k = 2, ...)
{
 # *** Gives AIC defined in Burnham and Anderson page 61 - not the same as R ***
 # *** Only for Gaussian ***
  n <- length(fit$residuals)
  edf <- n - fit$df.residual + 1
  dev <- fit$deviance
  if(scale > 0)
  dev <- dev/scale
  if(scale == 0 && fit$family$family == "gaussian") {
      cat("\nGausssian with scale = 0.0\n")
      cat("\nLog likelihood = ", (-n/2) * log(dev/n) , "\n\n")
      dev <- n * log(dev/n)
}
c(edf, dev + k * edf)
}

extractAIC.c.Burnham.Anderson.glm <- 
 # *** Gives AIC defined in Burnham and Anderson page 61 - not the same as R ***
 # *** Only for Gaussian ***
  function(fit, scale = 0, k = 2, ...) {
  n <- length(fit$residuals)
  edf <- n - fit$df.residual + 1
  dev <- fit$deviance
  if(scale > 0)
  dev <- dev/scale
  if(scale == 0 && fit$family$family == "gaussian")
  dev <- n * log(dev/n)
  c(edf, dev + k * edf + (2 * edf * (edf + 1))/(n - edf - 1))
}

extractQAIC.Burnham.Anderson.glm <-
function(fit, scale = 0, k = 2, ...) {
 # *** Gives AIC defined in Burnham and Anderson page 61 - not the same as R ***
 # *** Only for Gaussian ***
  n <- length(fit$residuals)
  edf <- n - fit$df.residual + 2   # One extra for estimating dispersion
  dev <- fit$deviance
  if(scale > 0)
  dev <- dev/scale
  if(scale == 0 && fit$family$family == "gaussian")
  dev <- n * log(dev/n)
  c(edf, dev + k * edf)
}

