function (x) 
{
    LL <- logLik(x)
    cat("\n")
    printf(LL)
    cat("\n")
    -2 * as.numeric(LL) + attributes(LL)$df * log(nrow(x$data))
}
