get.subs <- function(x, sep = ",", collapse=F) {
        subs <- function(x, sep = ",") {
        ' #   DATE WRITTEN:  26 May 1995,  Revised Apr 2017  '
        ' #   Author:  John R. Wallace (John.Wallace@noaa.gov)  '
        '  '
                if(length(sep) == 1) sep <- substring(sep, 1:nchar(sep), 1:nchar(sep))
                nc <- nchar(x)
                y <- (1:nc)[is.element(substring(x, 1:nc, 1:nc), sep)]
                if(is.na(y[1] + 0))
                        return(x)
                substring(x, c(1, y + 1), c(y - 1, nc))
        }
   '  '
   if(length(x) == 1 ) {
     if(collapse)
        paste(subs(x, sep=sep), collapse="")
     else
        subs(x, sep = sep)
   } else {
     if(collapse)
        apply(matrix(x, ncol=1), 1, function(x) paste(subs(x, sep=sep), collapse=""))
     else
        apply(matrix(x, ncol=1), 1, subs, sep = sep)
   }
 }

