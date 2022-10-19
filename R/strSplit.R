strSplit <- function (x, sep = ",", collapse = FALSE, elements = NULL, decimal_factor = 10000) 
{
    if (is.null(elements)) 
        Out <- apply(matrix(x, ncol = 1), MARGIN = 1, FUN = JRWToolBox::get.subs, 
            sep = sep, collapse = collapse)
            
    if(length(elements) == 1)         
       Out <- apply(matrix(x, ncol = 1), MARGIN = 1, FUN = function(y) {
            JRWToolBox::get.subs(y, sep = sep, collapse = collapse)[elements]})
            
    if(length(elements) == 2) {    
       Subs <- apply(matrix(x, ncol = 1), MARGIN = 1, FUN = function(y) {
            JRWToolBox::get.subs(y, sep = sep, collapse = collapse)[elements]}) 
       Out <- NULL     
       for(z in 1:ncol(Subs))      
          Out <- c(Out, as.numeric(Subs[1, z]) + as.numeric(Subs[2, z])/decimal_factor)
    }  
    
    Out    
}
