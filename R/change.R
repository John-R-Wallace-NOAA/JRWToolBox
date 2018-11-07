
change <- function (x) 
{
    tf <- ifelse(regexpr("Mydata", search()) == -1, F, T)
    if (any(tf)) {
        pos <- (1:length(search()))[tf]
        detach(pos = pos)
    }
    if (is.data.frame(x)) 
        cat("\nA data frame of size (", nrow(x), ", ", ncol(x), 
            ") has been attached in position 2.\n\n", sep = "")
    else {
        cat("\nAn object with the following structure has been attached in position 2:\n\n", 
            sep = "")
        str(x)
        cat("\n")
    }
    attach(x, 2, paste("Mydata:", substring(deparse(substitute(x))[1], 
        1, 30), sep = ""))
}
