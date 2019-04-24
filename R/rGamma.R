rGamma <- function(n, Mean, Var, nu = NULL, Shape.Scale = FALSE, verbose = FALSE, gammaFunc = rgamma) {
    
            "  # Dispersion is 1/nu, where nu is the shape parameter.  Dispersion is equal to the CV squared for the gamma.  "
       
                if(F) {  
                "  # Example of generating data for a glm model with gamma errors.  "
                   x <- seq(1,20, len=50000)
                   y <- rGamma(50000, exp(3 + 2*x), 4*(exp(3 + 2*x))^2) # 4 is Dispersion (nu = 1/Dispersion)
                   summary(glm(y ~ x, family = Gamma(link='log'))) # Estimate of Dispersion is given in summary()
                   y <- rGamma(50000, exp(3 + 2*x),  nu = 1/4) # Now using the nu shape argument, the variance argument is not used
                   summary(glm(y ~ x, family = Gamma(link='log'))) 
               }
       
               if(is.null(nu))
                       nu <- Mean^2/Var  "  # Since Dispersion = CV^2 = Var/Mean^2 = 1/nu  "
       
               if(verbose) {
                       catf("\nAverage Shape =", mean(1/nu), "Average Scale =", mean(nu*Mean), "\n\n")
                       plot(x, nu*Mean, col='green', ylim=c(0, max(c(1/nu, nu*Mean))))
                       points(x, 1/nu)
               }
       
               if(Shape.Scale)
                 list(Shape= nu, Scale = Mean/nu)
               else
                 gammaFunc(n, nu, scale = Mean/nu)  # rgamma(n, shape, rate = 1, scale = 1/rate)
       }
