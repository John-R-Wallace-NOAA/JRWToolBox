keypad <- function (as.num = TRUE, tolerance = 0.5) 
{
    index <- expand.grid(1:4, 1:4)
    padLab <- c(0, "", "", 7:9, 4:6, 1:3)
    numVec <- c(NA, 0, ".", NA, 7, 8, 9, NA, 4, 5, 6, NA, 1, 2, 3, NA)
    plot(c(0.7, 4.3), c(0.7, 4.3), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n")
    text(index[c(2:7, 9:11, 13:15), 1], index[c(2:7, 9:11, 13:15), 2], padLab, adj = 0)
    text(4, 4, "Finish", col= 'red', cex=1.3)
    text(3, 1, ".", cex = 4)
    NUM <- NULL
    while (TRUE) {
        newNum <- numVec[identify(index, lab = numVec, n = 1, plot = FALSE, tolerance = tolerance)]
        if (is.na(newNum))
            break
        NUM <- c(NUM, newNum)
        cat("\r", paste(NUM, collapse = ""))
        flush.console()
    }
    dev.off()
    cat("\n\n")
    flush.console()
    NUM <- paste(NUM, collapse = "")
    if (as.num) 
        NUM <- as.numeric(NUM)
    NUM
}
