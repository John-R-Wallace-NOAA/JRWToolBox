Str <- function(object, N = 4, ncols = 7, ...) {
   
   cat("\n\n")
   
    if(is.list(object) & is.data.frame(object)) {
       Cols <- min(c(7, ncol(object)))
       str(object[, 1:Cols], ...)
       lapply(object[, 1:Cols], head, N)
   }
   
   if(is.list(object) & !is.data.frame(object)) {
       str(object, ...)
       lapply(object, head, N)
   }
   
   if(!is.list(object) & !is.data.frame(object)) {
       str(object, ...)
       head(object, N)
   }    
}
