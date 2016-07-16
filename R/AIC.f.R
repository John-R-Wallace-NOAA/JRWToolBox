AIC.f <- function (x, k = 2) 
{
    LL <- logLik(x)
    print(LL)

    -2 * as.numeric(LL) + k * attributes(LL)$df
}
