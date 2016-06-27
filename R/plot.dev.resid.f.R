plot.dev.resid.f <- function (fit) 
{
    rs <- resid(fit, type = "deviance")
    plot(predict(fit), rs, xlab = "Linear Predictors", ylab = "Deviance Residuals")
    abline(h = 0, lty = 2)
    qqnorm(rs, ylab = "Deviance Residuals")
    qqline(rs)
    invisible()
}
