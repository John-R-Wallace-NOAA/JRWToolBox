renum <- function (x, no.num = F) 
{
    if(nrow(x) == 0)
         return(x)    
    if (no.num) 
        dimnames(x)[[1]] <- rep("", nrow(x))
    else dimnames(x)[[1]] <- 1:nrow(x)
    x
}

