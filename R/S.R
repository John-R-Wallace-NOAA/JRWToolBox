
S <- function(File, ...) {
    if (!(is.character(substitute(File))))
        File <- deparse(substitute(File))

     gitAFile(paste0("John-R-Wallace-NOAA/JRWToolBox/master/R/", File, ".R"), ...)     
}


