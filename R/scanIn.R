scanIn <- function (text, header = TRUE, ncol = 2, matrix = TRUE, numeric = FALSE) 
{
    if(matrix) {
        Out <- data.frame(matrix(scan(what = "", text = text, quiet = TRUE), 
              ncol = ncol, byrow = TRUE), stringsAsFactors = FALSE)
        if (header) {
            names(Out) <- as.character(Out[1, ])
            Out <- Out[-1, ]
        }
        Out <- JRWToolBox::renum(Out)
     } else {
        if(numeric)
            Out <- scan(text = text, quiet = TRUE)
        else 
            Out <- scan(what = "", text = text, quiet = TRUE)        
     }          
     Out
}         
