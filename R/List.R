List <- function(...) {

  tmp <- list(...)
  names <- as.character(substitute(list(...)))[-1]

  out <- list()

  for(i in 1:length(names)) {
    out[[names[i]]] <- tmp[[i]]
   }
 
  out
}

