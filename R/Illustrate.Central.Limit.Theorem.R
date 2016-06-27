Illustrate.Central.Limit.Theorem <- function (N = 10000, M = 5000) 
{
    graphics.off()
    par(mfrow = c(1, 2), pty = "s")
    for (k in 1:20) {
        m <- (rowMeans(matrix(runif(M * k), N, k)) - 0.5) * sqrt(12 * 
            k)
        hist(m, xlim = c(-4, 4), main = k, prob = TRUE, ylim = c(0, 
            0.5), col = "lemonchiffon")
        pu <- par("usr")[1:2]
        x <- seq(pu[1], pu[2], len = 500)
        lines(x, dnorm(x), col = "red")
        qqnorm(m, ylim = c(-4, 4), xlim = c(-4, 4), pch = ".", 
            col = "blue")
        abline(0, 1, col = "red")
        Sys.sleep(1)
    }
}
