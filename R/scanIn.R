scanIn <- function(text, header = TRUE, ncol = 2) {
    Out <- data.frame(matrix(scan(what = "", text = text, quiet = TRUE), ncol = ncol, byrow= TRUE))
    if(header) {
       names(Out) <- Out[1,]
	     Out <- Out[-1,]
    }
    JRWToolBox::renum(Out)
}

