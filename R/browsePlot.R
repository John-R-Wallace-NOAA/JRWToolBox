
browsePlot <- function(plotCode, width = 16, height = 10, res = 600, file = tempfile(fileext = ifelse(pdf, ".pdf", ".png")), 
                          browser = c("C:/Program Files (x86)/Google/Chrome/Application/chrome.exe", getOption("browser"))[1], pdf = FALSE) {

   switchSlash <- function (backSlash = readClipboard()) {
      forwardSlash <- gsub("//", "/", gsub("\\\\", "/", backSlash))
      cat(forwardSlash, file = "clipboard")
      forwardSlash
    }

 '  ---------------------------------------------------------------  '

 '  # unlink() needs to used with "recursive" arg set to TRUE to remove directories. Directories are moved out of temp areas e.g. by JRWToolBox::saveHtmlFolder() used to save ggplotly() HTML figure directories.  '       
    unlink(file, recursive = TRUE)
    
    if(pdf)
       pdf(width = width, height = height, file = file)
    else  
       png(width = width, height = height, units = 'in', res = res, file = file)
       
    eval(parse(text = plotCode))
    dev.off() 
    
    if(grepl(':/', switchSlash(file)))
       browseURL(file, browser = browser)
    else {
       cat("\n\nFigure saved at:", file, "\n")
       browseURL(sub(" ", "%20", paste0('file://', getwd(),'/', file)), browser = browser)
    } 

    if(!is.null(dev.list())) {
       cat("\n\nPlotting device list:\n")
       print(dev.list())    
    }
    
    invisible()
}
