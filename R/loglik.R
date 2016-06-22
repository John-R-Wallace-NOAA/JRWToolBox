loglik <- 
function(coef = fit$coefficients, fit, print.index = F) {

        
           N <- length(fit$y)
          
          mu <- fit$family$linkinv(model.matrix(fit) %*% coef)
        
         dev <- sum(fit$family$dev.resids(fit$y, mu, fit$prior.weights))

         out <- fit$family$aic(fit$y, N, mu, fit$prior.weights, dev)/-2
        
         if(fit$family$family %in% c("gaussian", "Gamma", "inverse.gaussian")) 
        
                out <- out + 1


        INDEX <<- INDEX + 1

        if(!(INDEX %% 200)) {

                if(print.index)
                        cat(INDEX, out)
                cat("\n\n")
                flush.console()
        }
        
        out
}

