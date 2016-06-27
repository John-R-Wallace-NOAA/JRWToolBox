AIC.f <- function (x, k = 2) 
{
    LL <- logLik(x)
    cat("\n")
    printf(LL)
    cat("\n")
    -2 * as.numeric(LL) + k * attributes(LL)$df
}
