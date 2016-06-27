function (nt, dim = 1, mn, std, crr, lg = F) 
{
    if (missing(mn)) 
        mn <- rep(0, dim)
    else mn <- rep(mn, len = dim)
    if (missing(std)) 
        std <- rep(1, dim)
    else std <- rep(std, len = dim)
    if (missing(crr)) 
        crr <- rep(0, (dim - 1))
    else crr <- rep(crr, len = dim - 1)
    xnor <- matrix(0, nt, dim)
    xnor[, 1] <- rnorm(nt)
    if (dim > 1) 
        for (idim in 2:dim) {
            tmp <- rnorm(nt)
            xnor[, idim] <- xnor[, (idim - 1)] * crr[idim - 1] + 
                sqrt(1 - crr[idim - 1]^2) * tmp
        }
    for (i in 1:dim) xnor[, i] <- xnor[, i] * std[i] + mn[i]
    if (lg) 
        xnor <- exp(xnor)
    xnor
}
