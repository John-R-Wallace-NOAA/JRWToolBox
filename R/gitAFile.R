gitAFile <- function (URL, run = FALSE, show = !run, type = c("function", "csv", "script", "RData")[1] , File = NULL, delete.R.Object = ifelse(type %in% 'function', TRUE, FALSE)) 
{
  
  '# Use the CDN jsDelivr name, i.e.: "https://cdn.jsdelivr.net/gh/John-R-Wallace/JRWToolBox@master/R/panel.conf.pred.band.R" '
  '# CDN jsDelivr homepage:  https://www.jsdelivr.com/'
  '# csv file download information was from here: http://www.r-bloggers.com/data-on-github-the-easy-way-to-make-your-data-available/'
  ''
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
    '# ------------------------------------'
  
    if (type %in% "csv") 
        return(read.csv(textConnection(getURL(URL))))
    if (type %in% c("function", "script") ) {
        if(is.null(File))
           File.ASCII <- tempfile()
        else 
           File.ASCII <- File
        writeLines(paste(readLines(textConnection(getURL(URL))), collapse = "\n"), File.ASCII)
        if(delete.R.Object)
           on.exit(file.remove(File.ASCII))
     }
     
     if(type %in% "function") {
           s.name <- Source(File.ASCII)
           if(run) 
             eval(parse(text = s.name))()
           if(show)
                 print(eval(parse(text = s.name)))
           return(invisible(eval(parse(text = s.name))))
      }
      if(type %in% "script") {
         if(run)
            source(File.ASCII)
         if(show)
            file.show(File.ASCII)
       }
       if(type %in% "RData") {
         ' # https://stackoverflow.com/questions/18833031/download-rdata-and-csv-files-from-ftp-using-rcurl-or-any-other-method '
         ' # test <- load(rawConnection(getBinaryURL(URL)))  # Does not work for me on binary RData files '
         File.BINARY <- tempfile()
         download.file(URL, File.BINARY, mode = 'wb')
         if(show)
            JRWToolBox::load(File.BINARY)
         if(!show)
            base::load(File.BINARY)
       }
}
