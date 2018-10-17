randomizeFileName <- function(n = 12, ext = "") {
   random.string <- rep(NA, n)
   randomizeString <- function(x) {
     a <-sample(letters, 1, replace = TRUE)
     return(a)
   }
   paste(c(sapply(random.string, randomizeString, simplify = TRUE), ext), collapse = "")
}


