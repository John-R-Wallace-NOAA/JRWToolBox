lsPath <- function(pattern) {

     if (!(is.character(substitute(pattern)))) {
        pattern <- deparse(substitute(pattern))
     }
     JRWToolBox::Ls( ,grep(paste0("\\", pattern, "\\b"), search()))
}
