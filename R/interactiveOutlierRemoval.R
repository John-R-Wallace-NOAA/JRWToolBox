interactiveOutlierRemoval <- function(xy) {

    '  # The x-axis data needs to be the first column of data and the y-axis data in the second column.  '
    '  # Either a data frame or a matrix object can be used.  '

    '%r1%' <- function (e1, e2) {
       ifelse(e1%%e2 == 0, e2, e1%%e2)
    }

    Cols <- sample(rainbow(20))
    
    X <- xy[, 1]
    Y <- xy[, 2]
    
    dev.new(width = 700, height = 500)
    plot(X, Y, type = 'p', xlab = ifelse(is.null(colnames(xy)[1]), "X", colnames(xy)[1]),
           ylab = ifelse(is.null(colnames(xy)[2]), "Y", colnames(xy)[2]))
    abline(Mod <- lm(Y ~ X), lwd = 1.5)
    cat("\nAIC =", AIC(Mod), "\n\n")
    
    N <- 1
    PtOutAll <- NULL
    while (length(PtOut <- identify(X, Y)) >= 1) {
    
        Col <- Cols[N%r1%20]
        points(X[PtOut], Y[PtOut], pch =19, col = Col)
        PtOutAll <- c(PtOutAll, PtOut)
        PtsIn <- !(1:length(X) %in% PtOutAll)
        abline(Mod <- lm(Y[PtsIn] ~ X[PtsIn]), lwd = 1.5, col = Col)
        cat("\nAIC =", AIC(Mod), "\n\n")
        N <- N + 1
   }

   out <- cbind(X[PtsIn], Y[PtsIn])
   colnames(out) <- colnames(xy)
   if(is.data.frame(xy))
      out <- data.frame(out)
   invisible(out)
}
