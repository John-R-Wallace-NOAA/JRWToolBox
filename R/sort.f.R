sort.f <- function(x, col = 1, reverse = F, renumber = T)
{
                
        if(is.null(x) | nrow(x) == 0)
            return(NULL)

        sort.col <- x[, col, drop=F]

        if(is.factor(sort.col))
                sort.col <- as.character(sort.col)

        if(!(is.list(sort.col)))
            sort.col <- list(sort.col)

        if(reverse)
            out <- x[rev(do.call(order, sort.col)),  ]
        else 
            out <- x[do.call(order, sort.col),  ]

        if(renumber) {
                if(is.data.frame(x))
                        dimnames(out)[[1]] <- 1:nrow(out)
        }
        out
}

if(F) {

sort.f <- function(x, col = 1, reverse = F, renumber = T)
{
        if(is.null(x))
            return(NULL)
        if(reverse)
                out <- x[rev(do.call(order, x[, col, drop=F])),  ]
        else out <- x[do.call(order, data.frame(x[, col, drop=F])),  ]
        if(renumber) {
                if(is.data.frame(x))
                        dimnames(out)[[1]] <- 1:nrow(out)
        }
        out
}

}
