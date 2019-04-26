

S2 <- function(File) {
    if (!(is.character(substitute(File))))
        File <- deparse(substitute(File))

     gitAFile2(paste0("John-R-Wallace-NOAA/JRWToolBox/master/R/", File, ".R"))     
}


