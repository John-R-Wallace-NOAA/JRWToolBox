col.alpha <- function (col, alpha = 0.5) {
      FUNC <- function(col, alpha = alpha) { 
             COL <- col2rgb(col)/255
             rgb(red = COL[1], green = COL[2], blue = COL[3], alpha = alpha)
       }
       for ( i in 1:length(col))
                col[i] <- FUNC(col[i], alpha = alpha[i %r1% length(alpha)])
       col
}


