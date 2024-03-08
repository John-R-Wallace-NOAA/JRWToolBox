
headTail <- function(x, n_head = 6L, n_tail = n_head) {
   rbind(head(x, n = n_head), tail(x, n = n_tail))
}   
