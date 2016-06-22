meanw <- function (x, weight = rep(1, length(x)), na.rm = F) 
{
    if(any(weight < 0))
        stop("All weights must be non-negative")
    if(sum(weight) == 0)
        return(NA)
    if (na.rm) {
        notna <- !(is.na(x) | is.na(weight))
        x <- x[notna]
        weight <- weight[notna]
    }
    else if (any(is.na(x))) 
        return(NA)
    x <- as.double(x)
    weight <- as.double(weight)
    sum(x * weight)/sum(weight)
}



varw <- function(x, weight = rep(1, length(x)), na.rm = F)
{
        if(any(weight < 0))
                 stop("All weights must be non-negative")
        if(sum(weight) == 0)
                return(NA)
	if(na.rm) {
		notna <- !(is.na(x) | is.na(weight))
		x <- x[notna]
		weight <- weight[notna]
	}
	else if(any(is.na(x)))
		return(NA)
	x <- as.double(x)
	weight <- as.double(weight)
	meanx <- sum(x * weight)/sum(weight)
	varx <- sum(weight * (x - meanx)^2)/(sum(weight) - 1)
	varx
}
