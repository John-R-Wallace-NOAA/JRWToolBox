
S <- function(File, show = TRUE, ...) {
    if (!(is.character(substitute(File))))
        File <- deparse(substitute(File))

     gitAFile(paste0("John-R-Wallace-NOAA/JRWToolBox/master/R/", File, ".R"), show = show, ...)     
}


