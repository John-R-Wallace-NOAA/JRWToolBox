DIC <- function(MCMC.iter, model.fit, verbose = FALSE) {

  
   DEV <- function(coef = fit$coefficients, fit) {
       
               N <- length(fit$y)
              mu <- fit$family$linkinv(model.matrix(fit) %*% coef)
             dev <- sum(fit$family$dev.resids(fit$y, mu, fit$prior.weights))
         loglik  <- fit$family$aic(fit$y, N, mu, fit$prior.weights, dev)/-2
         if(fit$family$family %in% c("gaussian", "Gamma", "inverse.gaussian"))
             loglik  <- loglik  + 1

         -2*loglik
   }

   if(!is.data.frame(MCMC.iter))
       MCMC.iter <- as.data.frame(MCMC.iter)
 
   DEV.vec <- apply(MCMC.iter[,1:model.fit$rank, drop = FALSE], 1, DEV, fit = model.fit)
   Dbar <- mean(DEV.vec)
   Dhat <- DEV(base::colMeans(MCMC.iter[,1:model.fit$rank, drop = FALSE]), fit = model.fit)

   if(verbose)  {

      cat("\nBayesian deviance from the model using -2 * logLik(fit): ", -2 * logLik(model.fit))
      cat("\nBayesian deviance, via finding -2 * log-likelihood the hard way, using the model fit: ",  DEV(fit = model.fit))
      cat("\nFirst 10 deviances from the MCMC: ", DEV.vec[1:10])
      cat("\nDbar: ", Dbar)
      cat("\nDhat: ", Dhat)
      cat("\nEffective number of parameters: ", Dbar - Dhat)
      cat("\nRank from the model fit: ", model.fit$rank, "\n\n")

   }    

   2*Dbar - Dhat  

}


