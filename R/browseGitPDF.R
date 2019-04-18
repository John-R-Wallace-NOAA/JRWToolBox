

browseGitPDF <- function(URL) {
    # https://webapps.stackexchange.com/questions/48061/can-i-trick-github-into-displaying-the-pdf-in-the-browser-instead-of-downloading
    cat("\nThe pdf is not saved locally until done so manually.\n\n")
    newURL <- paste0("http://nbviewer.jupyter.org/github/", paste0(strsplit(URL, "/")[[1]][-(1:3)], collapse="/"))
    browseURL(newURL)
    invisible(newURL)    
}

if(FALSE)  {

   # browseURL("https://github.com/dhimmel/delays/blob/icss/ICSS-2016/ICSS-2016-abstract.pdf")
   # browseGitPDF("https://github.com/dhimmel/delays/blob/icss/ICSS-2016/ICSS-2016-abstract.pdf")
   
   
   # Also inside of gitAFile()
   gitAFile("https://github.com/dhimmel/delays/blob/icss/ICSS-2016/ICSS-2016-abstract.pdf", "pdf")
   
   
   #  --------- Error -------------
   
   https://github.com/r-lib/remotes/issues/130
   
   options()$download.file.method
   
   ?download.file
   
   # Was not working even after a R restart of the same version and not working after a reinstall of RCurl and curl (but I didn't restart R)- a different R version did work
   options(download.file.method = "wininet") # Fixes issue
   options(download.file.method = "auto") # also works
   options()$download.file.method
   browseGitPDF("https://github.com/dhimmel/delays/blob/icss/ICSS-2016/ICSS-2016-abstract.pdf")
   
   
   # Working after closing and opening Windows or an R restart??
   browseGitPDF("https://github.com/dhimmel/delays/blob/icss/ICSS-2016/ICSS-2016-abstract.pdf")
   
   # The default - so should work and it does
   options(download.file.method = "libcurl")
   options()$download.file.method
   browseGitPDF("https://github.com/dhimmel/delays/blob/icss/ICSS-2016/ICSS-2016-abstract.pdf")

   
   
   #  --------- Round 2 with error -------------   
         
   > updateTools()
   Downloading GitHub repo John-R-Wallace/JRWToolBox@master
   Error in utils::download.file(url, path, method = download_method(), quiet = quiet,  : 
     cannot open URL 'https://api.github.com/repos/John-R-Wallace/JRWToolBox/tarball/master'
   
   # Gets fixed with:
   options(download.file.method = "auto")
   updateTools()
     
   # Error comes back with:  
   options(download.file.method = NULL)
   options()$download.file.method
   updateTools(force=T)  
      
   options(download.file.method = "auto")
   updateTools(force=TRUE) 
   
   
   options(download.file.method = NULL)
   options()$download.file.method
   remotes::install_github("John-R-Wallace/JRWToolBox", quiet = F, force = T)
   
   # Added options(download.file.method = "auto") to lib() - so now above error fixed
   
}






