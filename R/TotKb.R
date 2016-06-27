TotKb <- function () 
{
    require(gdata)
    Sum <- sum(ll()$KB)
    print(format(Sum, big.mark = ","), q = F)
    invisible(Sum)
}
