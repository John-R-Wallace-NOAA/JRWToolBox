tail.OLD <- function (x, len. = 15) 
{
    M <- is.matrix(x) | is.data.frame(x)
    if (M) 
        n <- nrow(x)
    else {
        if (is.vector(x)) 
            n <- length(x)
        else stop("Input must be a matrix or vector")
    }
    len. <- ifelse(n > len., len., n)
    ""
    if (M) 
        x[(n - len.):n, ]
    else x[(n - len.):n]
}
