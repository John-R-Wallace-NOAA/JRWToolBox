AIC.Log.y.with.Normal.Error <- function (FIT, k = 2, verbose = TRUE) 
{
    x <- resid(FIT)
    N <- length(x)
    LLC <- -sum((x - mean(x))^2)/(2 * var(x)) + N * log(1/sqrt(2 * 
        pi)) + N * log(1/sd(x))
    LL <- logLik(FIT)
    LNL <- LL + sum(log(1/exp(FIT$y)))
    if (verbose) {
        cat("\nLogliklihood; Calculated  =", LLC, "\n\n")
        cat("Logliklihood via logLik() function =", LL, "\n\n")
        cat("Lognormal logliklihood =", LNL, "\n\n")
    }
    df <- FIT$rank + 1
    AIC <- -2 * LNL + k * df
    cat("AIC = ", AIC, " (df = ", df, ")\n\n", sep = "")
    invisible(AIC)
}
