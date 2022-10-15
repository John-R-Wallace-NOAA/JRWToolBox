subStr <- function(x, sep = ",", collapse = FALSE, elements = NULL) {
   if(is.null(elements)) 
      apply(matrix(x, ncol = 1), MARGIN = 1, FUN = get.subs, sep = sep, collapse = collapse)
   else   
      apply(matrix(x, ncol = 1), MARGIN = 1, FUN = function(y) { get.subs(y, sep = sep, collapse = collapse)[elements] })
}
