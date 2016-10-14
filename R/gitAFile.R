
gitAFile <- function (URL, type = "function", File = NULL, delete.Object.R = TRUE) 
{
  
  '# Use the raw git name, i.e.: "https://raw.githubusercontent.com/John-R-Wallace/R-ToolBox/master/R/panel.conf.pred.band.R"'
  '# For raw GitHub URL see:  https://rawgit.com/'
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
  
    if (type == "csv") 
        return(read.csv(textConnection(getURL(URL))))
    if (type == "function") {
        if(is.null(File))
           File.ASCII <- "Object.R"
        else 
           File.ASCII <- File
        writeLines(paste(readLines(textConnection(getURL(URL))), collapse = "\n"), File.ASCII)
        if(delete.Object.R & is.null(File))
           on.exit(file.remove("Object.R"))
        if(is.null(File))
          Source("Object.R")
    }
}

