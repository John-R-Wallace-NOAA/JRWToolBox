function (x) 
{
    x[!is.finite(x)] <- 0
    if (all(x == 0)) 
        return(x)
    else x[x == 0] <- min(x[x > 0])/2
    log(x)
}
