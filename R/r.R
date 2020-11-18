r <- function (x, digits = 7)  {
    
    if(length(dim(x)) <= 2) {
       if (is.null(dim(x))) {
           if (is.numeric(x)) 
               round(x, digits = digits)
           else x
       } else {
                onlyMatrix <- is.matrix(x) & !is.data.frame(x)
                rowNames <- rownames(x)
                x <- JRWToolBox::renum(x)
                
                out <- data.frame(lapply(data.frame(x), function(y) if (is.numeric(y)) {
                    round(y, digits = digits) } else y))
                    
                if(onlyMatrix)
                    out <- as.matrix(out)
                    
                colnames(out) <- colnames(x)
                rownames(out) <- rowNames
                out
       }
    } else {
       out <- array(apply(x, 3, JRWToolBox::r, digits), dim(x)) 
       dimnames(out) <- dimnames(x)
       out
    }  
}


