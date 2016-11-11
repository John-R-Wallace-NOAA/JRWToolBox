col.alpha <- function (col, alpha = 0.5) {
   ' # Remainder function that gives back the divisor, not zero, when evenly divisible, e.g. 14 %r1% 7 gives 7 not 0 '
    assign("%r1%", function(e1, e2) ifelse(e1 %% e2 == 0, e2, e1 %% e2), pos = 1)
    ' '
    FUNC <- function(col, alpha = alpha) {
        COL <- col2rgb(col)/255
        rgb(red = COL[1], green = COL[2], blue = COL[3], alpha = alpha)
    }
    for (i in 1:length(col)) 
         col[i] <- FUNC(col[i], alpha = alpha[i %r1% length(alpha)])
    col
}
