scanIn <- function(text, header = TRUE, ncol = 2) {
    Out <- data.frame(matrix(scan(what = "", text = text, quiet = TRUE), ncol = ncol, byrow= TRUE), stringsAsFactors = FALSE)
    if(header) {
       names(Out) <- as.character(Out[1,])
	     Out <- Out[-1,]
    }
    JRWToolBox::renum(Out)
}

