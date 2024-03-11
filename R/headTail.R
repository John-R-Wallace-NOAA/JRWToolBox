
headTail <- function(x, n_head = 6L, n_tail = n_head, n_col_head = ifelse(nrow(x) > 50L, floor(n_head/2), NULL), n_col_tail = n_col_head) {

    out <- rbind(head(x, n = n_head), tail(x, n = n_tail))
    
    if(!is.null(n_col_head) & !is.null(n_col_tail) ) 
       out <- out[, c(1:n_col_head, (ncol(x) - n_col_tail + 1):ncol(x))]
    
    if(is.null(n_col_head) & !is.null(n_col_tail) ) 
       out <- out[, (ncol(x) - n_col_tail + 1):ncol(x)]
	   
    if(!is.null(n_col_head) & is.null(n_col_tail) ) 
       out <- out[, 1:n_col_head]
   
   out
}

