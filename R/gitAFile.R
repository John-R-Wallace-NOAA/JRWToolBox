
gitAFile <- function(URL) {

  # Use the raw git name, i.e.: "https://raw.githubusercontent.com/John-R-Wallace/R-ToolBox/master/R/AIC.f.R"
  # For GitHub see:  https://rawgit.com/

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

   sink('Object.R')
   cat(paste(readLines(textConnection(getURL(URL))), collapse = "\n"))
   sink()
   on.exit(file.remove('Object.R'))
   Source('Object.R')
   
}



