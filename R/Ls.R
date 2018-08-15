Ls <-function (pattern = "[!.]*", pos = 1, all = FALSE, long = FALSE, open = FALSE, fix = FALSE, ...) 
{
    if (all) 
        pos <- 1:length(search())
    
   if (!(is.character(substitute(pattern)))) {
       pattern <- deparse(substitute(pattern))
   }
   if (long) {
       for( i in pos) {
          cat("\n")
          print(ll(pos = i, pattern = pattern))
          cat("\n")
        }
        return(invisible())
    }
   else {
        out <- lapply(as.list(pos), objects, pat = pattern, ...)
        names(out) <- search()[pos]
   }
   xlen <- length(out)
   this <- paste("[[", names(out), "]]", sep = "")
   for (i in 1:xlen) {
       if (length(out[[i]]) > 0) {
           if (is.data.frame(out[[i]]) == TRUE) {
             if (dim(out[[i]])[1] > 0) {
               cat(this[i], ":\n", sep = "")
               print(out[[i]], prefix = this[i], q = FALSE)
               cat("\n")
             }
           }
           else {
             cat(this[i], ":\n", sep = "")
             print(out[[i]], prefix = this[i], q = FALSE)
             cat("\n")
           }
       }
   }
   if (open) {
        for (i in 1:length(out)) {
            cat("\n\n***", out[i], "***\n")
            print(eval(parse(text = out[i])))
        }
    }
    if (length(out) == 1 & fix == TRUE) 
        assign(substitute(out), edit(eval(parse(text = out))), 
            pos = 1)
    invisible(out)
}
