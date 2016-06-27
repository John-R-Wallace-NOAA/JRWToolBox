AIC.LogNormal <- function (FIT, k = 2) 
{
    x <- resid(FIT)
    N <- length(x)
    LL <- -sum((x - mean(x))^2)/(2 * var(x)) + N * log(1/sqrt(2 * 
        pi)) + N * log(1/sd(x))
    cat("\nLogliklihood =", LL, "\n\n")
    LL <- LL + sum(log(1/exp(FIT$y)))
    cat("\nLognormal logliklihood =", LL, "\n\n")
    df <- FIT$rank + 1
    AIC <- -2 * LL + k * df
    cat("AIC = ", AIC, " (df = ", df, ")\n\n", sep = "")
    invisible(AIC)
}
