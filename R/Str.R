Str <- function(object, N = 4, ...) {
   print(str(object, ...))
   cat("\n\n")
   if(is.list(object) & !is.data.frame(object))
       lapply(object, head, 4)
   else 
       head(object, N)
}



