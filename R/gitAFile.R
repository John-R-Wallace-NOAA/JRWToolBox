
gitAFile <- function(URL, type= "function") {

  '# Use the raw git name, i.e.: "https://raw.githubusercontent.com/John-R-Wallace/R-ToolBox/master/R/panel.conf.pred.band.R"'
  '# For raw GitHub URL see:  https://rawgit.com/'
  '# csv file download information was from here: http://www.r-bloggers.com/data-on-github-the-easy-way-to-make-your-data-available/'

  require(RCurl)

   Source <- function (file, ...) 
   {
       ls.ext <- function(file) {
           local({
               base::source(file, TRUE)
               base::ls()
           })
       }
       base::source(file, ...)
       ls.ext(file)
   }

   
   if(type == "csv") 
     return(read.csv(textConnection(getURL(URL))))

   if(type == "function") {
     sink('Object.R')
     cat(paste(readLines(textConnection(getURL(URL))), collapse = "\n"))
     sink()
     on.exit(file.remove('Object.R'))
     Source('Object.R')
   }
}



