
headTail <- function(x, n_head = 6L, n_tail = n_head, n_col_head = if(ncol(x) > 50) n_head else NULL, n_col_tail = n_col_head) {
        
        if(n_head + n_tail > nrow(x)) {
            n_head <- nrow(x)
                n_tail <- 0
        }   
        
    if(!is.null(n_col_head)) {  
           if(n_col_head + n_col_tail > ncol(x)) {
               n_col_head <- ncol(x)
                   n_col_tail <- 1
           }
    }
        
    out <- rbind(head(x, n = n_head), tail(x, n = n_tail))
    
    if(!is.null(n_col_head) & !is.null(n_col_tail) ) 
       out <- out[, c(1:n_col_head, (ncol(x) - n_col_tail + 1):ncol(x))]
    
    if(is.null(n_col_head) & !is.null(n_col_tail) ) 
       out <- out[, (ncol(x) - n_col_tail + 1):ncol(x)]
           
    if(!is.null(n_col_head) & is.null(n_col_tail) ) 
       out <- out[, 1:n_col_head]
   
    if(ifelse(is.null(n_col_head), 0, n_col_head) + ifelse(is.null(n_col_tail), 0, n_col_tail) > ncol(x))
	    warning('\n  *** More columns asked for then are in the data, extra columns may be shown, e.g. "column_name.1" ***\n')
   
   print(out)
   cat("\nDimension:", dim(x), "\n\n")
   
   invisible(out)
}

