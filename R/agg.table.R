agg.table <- function(df, Total.col = F, Print= TRUE, digits=7, sort.cols = 1, sortColNames = FALSE, rows.as.col = F) {
        
      sort.f <- function(x, col = 1, reverse = F, renumber = T) {
            if(reverse)
                out <- x[rev(do.call(order, x[, col, drop=F])),  ]
            else out <- x[do.call(order, data.frame(x[, col, drop=F])),  ]
            if(renumber) {
                if(is.data.frame(x))
                        dimnames(out)[[1]] <- 1:nrow(out)
             }
           out
       }

       renum <- function(x, no.num = F)
       {
            if(no.num)
                 dimnames(x)[[1]] <- rep("", nrow(x))
            else dimnames(x)[[1]] <- 1:nrow(x)
        x
       }

# -----------------------------------------------------------------------
       
        df <- sort.f(df, col = sort.cols)
        
        labels <- dimnames(df)[[2]]
        if(F) {
                if(!any(is.na(as.num(unique(df[[1]])))))
                        row <- sort(as.num(unique(df[[1]])))
                else row <- sort(unique(df[[1]]))
                ""
                if(!any(is.na(as.num(unique(df[[2]])))))
                        col <- sort(as.num(unique(df[[2]])))
                else col <- sort(unique(df[[2]]))
        }
        row <- unique(df[[1]])
        col <- unique(df[[2]])
        ""
        row.len <- length(row)
        col.len <- length(col)
        out <- matrix(NA, nrow = row.len, ncol = col.len, dimnames = list(as.character(row), as.character(col)))
        for(i in 1:row.len) {
                for(j in 1:col.len) {
                        try(out[i, j] <- df[df[[1]] == row[i] & df[[2]] == col[j], 3], silent = TRUE)
                }
        }

        out2 <- data.frame(out)
        dimnames(out2) <- dimnames(out)

        if(rows.as.col) {
             out2 <- cbind(row, out2)
             names(out2)[1] <- names(df)[1]
             out2 <- renum(out2)
        }
                    
        if(sortColNames) 
            out2 <- out2[ ,order(names(out2))]
        
        if(Print) {
             cat("\n\t\t", labels[3], "\n\n", labels[1], "\t\t\t", labels[2], "\n\n")
             print(r(out2, digits = digits))
             cat("\n\n")
        }
        
        if(Total.col)
             cat("Total", apply(out, 2, sum, na.rm=T), "\n\n")

        invisible(out2)
}


