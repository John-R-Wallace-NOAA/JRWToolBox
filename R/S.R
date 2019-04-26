
S <- function(File) {
    if (!(is.character(substitute(File))))
        File <- deparse(substitute(File))

     JRWToolBox::gitAFile(paste0("John-R-Wallace-NOAA/JRWToolBox/master/R/", File, ".R"))     
}


