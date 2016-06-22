r <- function(x, digits = 7)
{
       if(is.null(dim(x)))
               if(is.numeric(x))
                       round(x, digits=digits)
               else x
       else { 
               out <- data.frame(lapply(data.frame(x), function(y)
                    if(is.numeric(y)) round(y, digits=digits) else y))
               colnames(out) <- colnames(x)
               rownames(out) <- rownames(x)
               out
       }
}

