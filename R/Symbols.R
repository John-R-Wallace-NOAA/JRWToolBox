function () 
{
    Pex <- 3
    ipch <- 1:(np <- 25 + 11)
    k <- floor(sqrt(np))
    dd <- c(-1, 1)/2
    rx <- dd + range(ix <- (ipch - 1)%/%k)
    ry <- dd + range(iy <- 3 + (k - 1) - (ipch - 1)%%k)
    pch <- as.list(ipch)
    pch[25 + 1:11] <- as.list(c("*", ".", "o", "O", "0", "+", 
        "-", ":", "|", "%", "#"))
    plot(rx, ry, type = "n", axes = FALSE, xlab = "", ylab = "", 
        main = paste("plot symbols :  points (...  pch = *, cex =", 
            Pex, ")"))
    abline(v = ix, h = iy, col = "lightgray", lty = "dotted")
    for (i in 1:np) {
        pc <- pch[[i]]
        points(ix[i], iy[i], pch = pc, col = "red", bg = "yellow", 
            cex = Pex)
        text(ix[i] - 0.3, iy[i], pc, col = "brown", cex = 1.2)
    }
}
