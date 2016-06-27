function (AIC, digits = 7) 
{
    cat("\n")
    delta <- AIC - min(AIC)
    wght.evd <- exp(-0.5 * (delta))/sum(exp(-0.5 * (delta)))
    data.frame(AIC = AIC, Delta = delta, Wght.of.Evidence = wght.evd)
}
