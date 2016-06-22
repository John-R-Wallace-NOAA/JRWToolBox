get.subs <- function(x, sep = ",") {
        subs <- function(x, sep = ",")
        {
        #   DATE WRITTEN:  26 May 1995, Revised Dec. 1998
        #   Author:  John R. Wallace (jw@u.washington.edu)
        #
                if(length(sep) == 1) sep <- substring(sep, 1:nchar(sep), 1:nchar(sep))
                nc <- nchar(x)
                y <- (1:nc)[is.element(substring(x, 1:nc, 1:nc), sep)]
                if(is.na(y[1] + 0))
                        return(x)
                substring(x, c(1, y + 1), c(y - 1, nc))
        }

if(len(x) == 1)
        subs(x, sep = sep)
else
        apply(matrix(x, ncol=1), 1, subs, sep = sep)
}
