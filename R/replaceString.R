
replaceString <- function(x, oldString, newString) {
   unlist(lapply(strsplit(x, split = oldString), paste, collapse = newString))
}
