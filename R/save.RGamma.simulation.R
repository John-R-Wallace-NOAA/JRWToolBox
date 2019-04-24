save.RGamma.simulation <- function (N = 500) {
  
    x <- seq(1, 3, len = N)
    y <- rGamma(N, exp(1 + 2.5 * x), 0.3 * exp(1 + 2.5 * x)^2)
    gm <- glm(y ~ x, family = Gamma(link = log))
    cat("\nSummary of glm(y ~ x, family = Gamma(link = log))\n\n")
    print(summary(gm))
    cat("AIC =", AIC(gm), "\n")
    cat("\n\n\nSummary the same model using MASS's gamma.dispersion() function\n\n")
    summary(gm, dispersion = gamma.dispersion(gm))
    JRWToolBox::lib(gamlss, attach = FALSE)
    gl <- gamlss::gamlss(y ~ x, family = LOGNO2)
    cat("\n\n\nSummary of gamlss::gamlss(y ~ x, family = LOGNO2))\n\n")
    print(summary(gl))
    plot(x,y)
    lines(x, exp(1 + 2.5 * x), lwd=2)
    lines(x, fitted(gm), col = 'green', lwd=2)
    lines(x, fitted(gl), col = 'cyan', lwd=2)
   
}    
    
    
    
    
