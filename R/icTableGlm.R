
icTableGlm <- function(GLM, second.ord = TRUE, nobs = NULL, sort = FALSE, BIC = FALSE, verbose = TRUE) {

  require(AICcmodavg)
  
  Y <- as.character(GLM$formula)[2]
  Xvar <- strsplit(as.character(GLM$formula)[3], '+', fixed = TRUE)[[1]]
  
  if(BIC)
     icFunc <- AICcmodavg::bictab
  if(!BIC)    
     icFunc <- AICcmodavg::aictab
  
  mod <- list()
  modnames <- NULL
  
  for(i in 1:length(Xvar)) {
  
    mod[[i]] <- glm(formula(paste(Y, "~", paste(Xvar[1:i], collapse = "+"))), data = GLM$data, family = as.character(GLM$call)[3]) 
    modnames <- c(modnames, paste(Xvar[1:i], collapse = "+"))
    if(verbose) {
       formula <- as.character(mod[[i]]$formula)
       cat("\n", formula[2], formula[1], formula[3])
       if(BIC)
         cat("\n BIC:", AICcmodavg::useBIC(mod[[i]]), "\n\n")
       if(!BIC)
         cat("\n AIC:", AICcmodavg::AICc(mod[[i]]), "\n\n")  
     }  
  }
  
  icFunc(mod, modnames, second.ord = second.ord, nobs = nobs, sort = sort)

}  

