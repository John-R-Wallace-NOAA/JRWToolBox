
gitAFile <- function (URL, type = c("function", "csv", "script", "RData", "RPckageZip", "pdfGitHub")[1], File = NULL, shortName = NULL, run = FALSE, show = FALSE, viewOnly = FALSE, 
                      deleteFileObj = ifelse(is.null(File), TRUE, FALSE), rawGitPrefix = TRUE, verbose = FALSE, ...) 
{
  # Example:  gitAFile("John-R-Wallace-NOAA/JRWToolBox/master/R/gitAFile.R")
  # Adds the raw GitHub prefix to create a full URL when type = "function", i.e.: paste0("https://raw.githubusercontent.com", "John-R-Wallace-NOAA/JRWToolBox@master/R/panel.conf.pred.band.R") 
  # Set rawGitPrefix = FALSE to not add a prefix.
    
  # Example of pdf download: gitAFile("https://github.com/James-Thorson/VAST/blob/master/manual/VAST_model_structure.pdf", "pdf")
  # Or directly using browseGitPDF: JRWToolBox::browseGitPDF("https://github.com/James-Thorson/VAST/blob/master/manual/VAST_model_structure.pdf")
  # Displaying pdf's from GitHub: https://webapps.stackexchange.com/questions/48061/can-i-trick-github-into-displaying-the-pdf-in-the-browser-instead-of-downloading 
  
    JRWToolBox::lib(RCurl)
    Source <- function(file, ...) {
        ls.ext <- function(file) {
            local({
                base::source(file, TRUE)
                base::ls()
            })
        }
        base::source(file, ...)
        ls.ext(file)
    }
    # ------------------------------------
    
    URL <- paste(strsplit(URL," ")[[1]], collapse = '%20')
    
    if(rawGitPrefix) 
           URL <- paste0('https://raw.githubusercontent.com/', URL)
           
    if(verbose) {
           cat("\n\n"); print(URL); cat("\n\n")
    }
    
    if(grepl(type, "csv"))         
        return(read.csv(textConnection(getURL(URL))))
  
    
    if(grepl(type, "function") | grepl(type, "script")) {
        if(is.null(File))
           File.ASCII <- tempfile()
        else 
           File.ASCII <- File
        writeLines(paste(readLines(textConnection(getURL(URL))), collapse = "\n"), File.ASCII)
        if(deleteFileObj)
           on.exit(file.remove(File.ASCII), add = TRUE)
    } 
    
    if(verbose) 
           cat("\n\n"); print(URL); cat("\n\n")
     
    if(grepl(type, "function")) {
          s.name <- Source(File.ASCII)
          print(s.name)
          if(run) 
            eval(parse(text = s.name), envir = globalenv())(...)
          if(show)
             print(eval(parse(text = s.name), envir = globalenv()))
          if(viewOnly)   
             on.exit(rm(list = s.name, pos = globalenv()), add = TRUE)
          return(invisible(eval(parse(text = s.name), envir = globalenv())))
    }
     
    if(grepl(type, "script")) {
       if(run)
          source(File.ASCII)
       if(show)
          file.show(File.ASCII)
    }
     
    if(any(type %in% c("RData", "RPckageZip"))) {
        # https://stackoverflow.com/questions/18833031/download-rdata-and-csv-files-from-ftp-using-rcurl-or-any-other-method 
        # test <- load(rawConnection(getBinaryURL(URL)))  # Does not work for me on binary RData files 
        
        if(is.null(File)) {
          if(type %in% "RPckageZip") 
             File.BINARY <- tempfile(fileext = ".zip")
          if(type %in% "RData")
             File.BINARY <- tempfile()        
       } else {
          File.BINARY <- File
    }
        
       download.file(URL, File.BINARY, mode = 'wb')
       
    if(type %in% "RData") {
          if(show)
             JRWToolBox::load(File.BINARY)
          if(!show)
             base::load(File.BINARY)
    }
       
    if(type %in% "RPckageZip") {
          # install_local(File.BINARY)
          unzip(File)
          noZipName <- get.subs(File, sep = ".")
          noZipName <- paste(noZipName[-length(noZipName)], collapse = ".")
          if(is.null(shortName)) {
               shortName <- get.subs(noZipName, sep = "-")
               shortName <- paste(shortName[-length(shortName)], collapse = "-")
               file.rename(noZipName, shortName)
          }     
          if(!is.null(shortName))
             setwd(noZipName)
          zip(paste0(shortName, ".zip"), files = shortName)
          install.packages(paste0(shortName, ".zip"),  repos = NULL, type = "win.binary")
          if(!is.null(shortName))
             setwd("..")
       }
    }
     
    if(grepl(type, "pdfGitHub")) { 
        JRWToolBox::browseGitPDF(URL)
    }
     
}


