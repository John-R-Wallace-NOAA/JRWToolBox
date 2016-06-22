match.f <- function(file, table, findex = 1, tindex = findex, tcol = 2, round. = T, digits=0)
{
#
#   DATE WRITTEN:  ???      LAST REVISED:   7 March 2000
#   AUTHOR:  John R. Wallace (John.Wallace@noaa.gov)
#
        paste.col <- function(x)
        {
                if(is.null(dim(x)))
                        return(paste(as.character(x)))
                out <- paste(as.character(x[, 1]))
                for(i in 2:ncol(x)) {
                        out <- paste(out, as.character(x[, i]))
                }
                out
        }
        if(is.null(dim(file))) {
                dim(file) <- c(length(file), 1)
        }
        if(round.) {
                for(i in findex) {
                        if(is.numeric(file[, i]))
                                file[, i] <- round(file[, i], digits)
                }
                for(i in tindex) {
                        if(is.numeric(table[, i]))
                                table[, i] <- round(table[, i], digits)
                }
        }
        cbind(file, table[match(paste.col(file[, findex]), paste.col(table[, tindex])), tcol, drop = F])
}

