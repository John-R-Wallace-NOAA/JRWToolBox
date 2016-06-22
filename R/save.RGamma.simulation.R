save.RGamma.simulation <- function(N=50000)
{
    x <- seq(1, 3, len = N)

# Alpha = 1, Beta = 2.5 for the GLM

# The variance has been set to 0.3 times the mean squared, that is the dispersion has been set to 0.3

# In terms of the shape parameter nu: nu = 1/CV^2 = mean^2/var = mean^2/(0.3*mean^2) = 1/0.3 , and so dispersion is 1/nu = 0.3 

    y <- RGamma(N, exp(1 + 2.5 * x), 0.3 * exp(1 + 2.5 * x)^2)

    gm <- glm(y ~ x, family = Gamma(link = log))

    print(summary(gm))

    cat("\n Summary using MASS's gamma.dispersion() function\n\n")
    summary(gm, dispersion = gamma.dispersion(gm))
}



