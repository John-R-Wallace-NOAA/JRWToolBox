

browseGitPDF <- function(URL) {
    ' # https://webapps.stackexchange.com/questions/48061/can-i-trick-github-into-displaying-the-pdf-in-the-browser-instead-of-downloading '
    cat("\nThe pdf is not saved locally until done so manually.\n\n")
    newURL <- paste0("http://nbviewer.jupyter.org/github/", paste0(strsplit(URL, "/")[[1]][-(1:3)], collapse="/"))
    browseURL(newURL)
    invisible(newURL)    
}







