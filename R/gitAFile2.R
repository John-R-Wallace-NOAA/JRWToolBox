gitAFile <- function (URL, run = FALSE, show = !run, type = "function", File = NULL, delete.R.Object = ifelse(type %in% 'function', TRUE, FALSE)) 
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
                 eval(parse(text = s.name))
             else
                 s.name
      }
      if(type %in% "script") {
         if(run)
            source(File.ASCII)
          if(show)
            file.show(File.ASCII)
       }
}
