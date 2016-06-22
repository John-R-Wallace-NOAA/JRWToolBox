RGamma <- 
function(n, Mean, Var, nu = NULL, Shape.Scale = FALSE, verbose = FALSE)
{
        # Dispersion is 1/nu, where nu is the shape parameter.  Dispersion is equal to the CV squared for the gamma.

         if(F) {  
          # Example of generating data for a glm model with gamma errors.
            x <- seq(1,20, len=50000)
            y <- RGamma(50000, exp(3 + 2*x), 4*(exp(3 + 2*x))^2) # 4 is dispersion
            y <- RGamma(50000, exp(3 + 2*x), (exp(3 + 2*x))^2, 4) # Now using the dispersion argument
            summary(glm(y ~ x, family = Gamma(link='log'))) # Estimate of dispersion is given in summary()
        }

        if(is.null(nu))
                nu <- Var/Mean^2

        if(verbose) {
                catf("\nAverage Shape =", mean(1/nu), "Average Scale =", mean(nu*Mean), "\n\n")
                plot(x, nu*Mean, col='green', ylim=c(0, max(c(1/nu, nu*Mean))))
                points(x, 1/nu)
        }

        if(Shape.Scale)
          list(Shape= 1/nu, Scale = (nu*Mean))
        else
           rgamma(n, 1/nu, 1/(nu*Mean))
}
 

