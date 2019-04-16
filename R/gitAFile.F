gitAFile <- function (URL, type = c("function", "csv", "script", "RData", "pdfGitHub")[1], run = FALSE, show = !run, File = NULL, delete.R.Object = ifelse(type %in% 'function', TRUE, FALSE)) 
{
  
  # Use the CDN jsDelivr name, i.e.: "https://cdn.jsdelivr.net/gh/John-R-Wallace/JRWToolBox@master/R/panel.conf.pred.band.R" 
  # CDN jsDelivr homepage:  https://www.jsdelivr.com/
  # csv file download information was from here: http://www.r-bloggers.com/data-on-github-the-easy-way-to-make-your-data-available/ 
  # Displaying pdf's from GitHub: https://webapps.stackexchange.com/questions/48061/can-i-trick-github-into-displaying-the-pdf-in-the-browser-instead-of-downloading 
  
    require(RCurl)
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
  
    if (grepl(type, "csv")) 
        return(read.csv(textConnection(getURL(URL))))
        
    if (grepl(type, "function") | grepl(type, "script") ) {
        if(is.null(File))
           File.ASCII <- tempfile()
        else 
           File.ASCII <- File
        writeLines(paste(readLines(textConnection(getURL(URL))), collapse = "\n"), File.ASCII)
        if(delete.R.Object)
           on.exit(file.remove(File.ASCII))
     }
     
     if(grepl(type, "function")) {
           s.name <- Source(File.ASCII)
           if(run) 
             eval(parse(text = s.name))()
           if(show)
                 print(eval(parse(text = s.name)))
           return(invisible(eval(parse(text = s.name))))
      }
      
      if(grepl(type, "script")) {
         if(run)
            source(File.ASCII)
         if(show)
            file.show(File.ASCII)
       }
       
       if(grepl(type, "RData")) {
         ' # https://stackoverflow.com/questions/18833031/download-rdata-and-csv-files-from-ftp-using-rcurl-or-any-other-method '
         ' # test <- load(rawConnection(getBinaryURL(URL)))  # Does not work for me on binary RData files '
         if(is.null(File))
            File.BINARY <- tempfile()
         else 
            File.BINARY <- File
         download.file(URL, File.BINARY, mode = 'wb')
         if(show)
            JRWToolBox::load(File.BINARY)
         if(!show)
            base::load(File.BINARY)
       }
       
       if(grepl(type, "pdfGitHub")) { 
          JRWToolBox::browseGitPDF(URL)
       }
}
