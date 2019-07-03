Run <- function (File, run = TRUE, viewOnly = TRUE, ...) 
{
    if (!(is.character(substitute(File)))) 
        File <- deparse(substitute(File))
    gitAFile(paste0("John-R-Wallace-NOAA/JRWToolBox/master/R/", 
        File, ".R"), run = run , viewOnly = viewOnly, ...)
}

# Run(Ls, pat='bds')

